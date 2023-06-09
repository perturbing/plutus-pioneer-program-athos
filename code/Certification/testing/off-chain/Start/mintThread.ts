import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts"
import { secretSeed } from "../seed.ts";
import merkleData from "../../../data/start-merkleTree-Data.ts";

import * as mod from "https://deno.land/std@0.182.0/crypto/mod.ts";
import {decode} from "https://deno.land/std/encoding/hex.ts";

// function flipBit(hexStr: string, bitIndexLE: number): string {
//   // Convert the hex string to a binary string
//   let binaryStr = parseInt(hexStr, 16).toString(2).padStart(hexStr.length * 4, '0');
//   let bitIndex = hexStr.length*4 - bitIndexLE - 1

//   // Check if the bit at the specified index is set
//   if (binaryStr[bitIndex] === '1') {
//       // If it is, clear it
//       binaryStr = binaryStr.substring(0, bitIndex) + '0' + binaryStr.substring(bitIndex + 1);
//   } else {
//       // If it isn't, set it
//       binaryStr = binaryStr.substring(0, bitIndex) + '1' + binaryStr.substring(bitIndex + 1);
//   }

//   // Convert the binary string back to a hex string, keeping leading zeroes
//   let hexResult = parseInt(binaryStr, 2).toString(16).padStart(hexStr.length, '0');

//   return hexResult;
// }

function flipBit(hexStr: string, bitIndex: number): string {
  // Parse the hex string as a BigInt
  let num = BigInt('0x' + hexStr);

  // Create a mask with a 1 at the desired position
  let mask = BigInt(1) << BigInt(bitIndex);

  // XOR the number with the mask to flip the bit at the desired position
  let result = num ^ mask;

  // Convert the result back to a hex string
  let resultStr = result.toString(16);

  // Pad the result with leading zeros to match the original string length
  while (resultStr.length < hexStr.length) {
      resultStr = '0' + resultStr;
  }

  return resultStr;
}

// cardano uses blake2b hash function for plutus data
async function blake2bHash(input:string): Promise<string> {
  const digest = await mod.crypto.subtle.digest(
    "BLAKE2B-256",
    decode(new TextEncoder().encode(input))
  );
  const string = mod.toHashString(digest)
  return string
}

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

// define here your public key hash, participant id and exam script cbor here
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;
const participantId: number = 2001;
const scriptCBOR = "49480100002221200101"
// the validator where the participant should lock their thread token
const lockingValidatorParticipant: L.SpendingValidator = {
  type: "PlutusV2",
  script: scriptCBOR
}
const lockingAddressParticipant: L.Address = lucid.utils.validatorToAddress(lockingValidatorParticipant);

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

// get for above use a merkle tree proof
const member = L.fromText(merkleData[participantId])
const merkleProof : Types.MerkleProof = merkleTree.getProof(dataUint[participantId]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

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

const stateParameters: Types.StateParameters = {
  threadSymbol: threadPol,
  finalState: "f".repeat(1000)
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

const Redeemer = L.Data.Object({
  proof: Types.MerkleProof,
  pkh:   Types.PubKeyHash,
  n:     L.Data.Integer()
})
type Redeemer = L.Data.Static<typeof Redeemer>

const redeemer: Redeemer = {proof: merkleProof, pkh: pkh, n: BigInt(participantId)}
async function mint(): Promise<L.TxHash> {
  const tkn: L.Unit = L.toUnit(policyIdFree, L.fromText("PPP Cert State Token"));
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(stateAddress);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[tkn] == 1n);
  const threadTkn: L.Unit = L.toUnit(threadPol,pkh);

  if (ourUTxO && ourUTxO.length > 0) {
    const stateDatum = await lucid.provider.getDatum(ourUTxO[0].datumHash)
    console.log("old state: "+L.Data.from(stateDatum))
    const newState = flipBit(L.Data.from(stateDatum),participantId)
    console.log("new state: "+ newState)

    const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(0,[])))
    .attachSpendingValidator(stateValidator)
    .payToContract(stateAddress, L.Data.to(newState),{[tkn]: 1n})
    .mintAssets({ [threadTkn]: 1n }, L.Data.to<Redeemer>(redeemer,Redeemer))
    .attachMintingPolicy(threadScript)
    .payToContract(lockingAddressParticipant, L.Data.to(pkh),{[threadTkn]: 1n})
    .addSignerKey(pkh)
    .complete({ nativeUplc: false });
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
  }
  else return "No UTxO's found that can be burned"
}
console.log(await mint())
