import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts";
import startMerkleData from "../../../data/start-merkleTree-Data.ts";
import mintMerkleData from "../../../data/mint-merkleTree-Data.ts";

// set blockfrost endpoint
const lucid = await L.Lucid.new(
  undefined,
  "Preview"
);

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
const startDataUint = startMerkleData.map((x) => L.fromHex(x));
const startMerkleTree = new L.MerkleTree(startDataUint);
const startMerkleRoot: Types.Hash = { hash: L.toHex(startMerkleTree.rootHash())};

// setup the thread parameter from the above merkle tree
const threadParameters: Types.ThreadParameters = {
    merkleRoot: startMerkleRoot,
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
    finalState: "ff".repeat(1001)
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

// import the locking validator (this locks the reference token of the PPP Certificate NFT)
const lockingValidator: L.SpendingValidator = await readScript("lockingValidator.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);
const lockingAddressDetails: L.AddressDetails = L.getAddressDetails(lockingAddress);

// mint merkle tree stuff
const mintDataUint = mintMerkleData.map((x) => L.fromHex(x));
const mintMerkleTree = new L.MerkleTree(mintDataUint);
const mintMerkleRoot: Types.Hash = { hash: L.toHex(mintMerkleTree.rootHash())};

// setup parameters
const prefixNFT: Types.Prefix = L.toLabel(222);
const prefixRef: Types.Prefix = L.toLabel(100);
const lockAddr: Types.Address = {addressCredential: { ScriptCredential: [lockingAddressDetails.paymentCredential.hash] }, addressStakingCredential: null};

const parameters: Types.Parameters = {
    merkleRoot: mintMerkleRoot,
    prefixNFT: prefixNFT,
    prefixRef: prefixRef,
    threadSymbol: threadPol,
    lockAddress: lockAddr
}

// import NFT minting policy and apply above parameters
const Params = L.Data.Tuple([Types.Parameters]);
type Params = L.Data.Static<typeof Params>;
async function readNFTPolicy(): Promise<L.MintingPolicy> {
  const script = JSON.parse(await Deno.readTextFile("assets/certificate-policy.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<Params>(script.cborHex,[parameters],Params)
  }
}
const mintingScriptNFT: L.MintingPolicy = await readNFTPolicy();
const policyIdNFT: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptNFT);
console.log("NFT PolicyID: "+policyIdNFT)

const setup = {
  stateToken: L.toUnit(policyIdFree, L.fromText("PPP Cert State token")),
  threadScript: threadScript,
  stateValidator: stateValidator,
  mintingScriptNFT: mintingScriptNFT 
}

const setupData = `export default ${JSON.stringify(setup)};`;
// write the data to the data folder.
Deno.writeTextFileSync("./data/setupData.ts", setupData);