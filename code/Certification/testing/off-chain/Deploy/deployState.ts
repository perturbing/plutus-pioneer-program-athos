import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts";
import merkleData from "../../../data/start-merkleTree-Data.ts";
import { secretSeed } from "../seed.ts";

// set blockfrost endpoint
const lucid = await L.Lucid.new(
  new L.Blockfrost(
    "https://cardano-preview.blockfrost.io/api/v0",
    "previewPZPBTOn0LUBOGMxM441DbnXT02Qwj4Ri"
  ),
  "Preview"
);

lucid.selectWalletFromSeed(secretSeed);
const addr: L.Address = await lucid.wallet.address();
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// import always true minting policy (replace this for a real NFT policy for the state token)
const mintingScriptFree: L.MintingPolicy = await readScript("alwaysTrue-policy.plutus");
const policyIdFree: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptFree);

// start merkle tree stuff
const dataUint = merkleData.map((x) => L.fromHex(x));
const merkleTree = new L.MerkleTree(dataUint);
const merkleRoot: Types.Hash = { hash: L.toHex(merkleTree.rootHash())};

// setup the thread parameter from the above merkle tree
const threadParameters: Types.ThreadParameters = {
    merkleRoot: merkleRoot,
    stateSymbol: policyIdFree,
}

// import Thread minting policy and apply above parameters
const ThreadParams = L.Data.Tuple([Types.ThreadParameters]);
type ThreadParams = L.Data.Static<typeof ThreadParams>;
async function readThreadPol(): Promise<L.MintingPolicy> {
  const script = JSON.parse(await Deno.readTextFile("assets/start-policy.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<ThreadParams>(script.cborHex,[threadParameters],ThreadParams)
  }
}
const threadScript: L.MintingPolicy = await readThreadPol();
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);
console.log("Thread policyID: "+threadPol)

// setup the state parameter from the thread minting policy and final state
const stateParameters: Types.StateParameters = {
    threadSymbol: threadPol,
    finalState: "ff".repeat(1000)
}

const StateParams = L.Data.Tuple([Types.StateParameters]);
type StateParams = L.Data.Static<typeof StateParams>;
async function readStateVal(): Promise<L.SpendingValidator> {
  const script = JSON.parse(await Deno.readTextFile("assets/start-state-validator.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<StateParams>(script.cborHex,[stateParameters],StateParams)
  }
}
const stateValidator: L.SpendingValidator = await readStateVal();
const stateAddress: L.Address = lucid.utils.validatorToAddress(stateValidator);
console.log("State addr "+ stateAddress)

async function initState(): Promise<L.TxHash> {
    const tkn: L.Unit = L.toUnit(policyIdFree, L.fromText("PPP Cert State Token"));
    const tx = await lucid
      .newTx()
      .mintAssets({ [tkn]: 1n}, L.Data.void())
      .payToContract(stateAddress, L.Data.to("00".repeat(1000)),{[tkn]: 1n})
      .attachMintingPolicy(mintingScriptFree)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}

// console.log(await initState())