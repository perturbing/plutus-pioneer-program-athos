import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import setupData from "../../../data/setupData.ts";
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

const threadScript: L.MintingPolicy = setupData.threadScript;
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);
console.log("Thread policyID: "+threadPol)

const stateValidator: L.SpendingValidator = setupData.stateValidator;
const stateAddress: L.Address = lucid.utils.validatorToAddress(stateValidator);
console.log("State addr "+ stateAddress)

async function initState(): Promise<L.TxHash> {
    const tkn: L.Unit = setupData.stateToken;
    const tx = await lucid
      .newTx()
      .mintAssets({ [tkn]: 1n}, L.Data.void())
      .payToContract(stateAddress, L.Data.to("00".repeat(1001)),{[tkn]: 1n})
      .attachMintingPolicy(mintingScriptFree)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}

console.log(await initState())