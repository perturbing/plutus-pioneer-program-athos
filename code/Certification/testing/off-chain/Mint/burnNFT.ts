import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts";
import { secretSeed } from "../seed.ts";
import genericMetadata from "../../../data/generic-NFT-metadata.json" assert { type: "json" };
import merkleData from "../../../data/mint-merkleTree-Data.ts";
import * as mod from "https://deno.land/std@0.182.0/crypto/mod.ts";
import {decode} from "https://deno.land/std/encoding/hex.ts";

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

// define here your name and public key hash
const particpantsName: string = "name1"
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// import always true minting policy (replace for threadtoken policy)
const mintingScriptFree: L.MintingPolicy = await readScript("alwaysTrue-policy.plutus");
const policyIdFree: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptFree);

// import the locking validator (this locks the reference token)
const lockingValidator: L.SpendingValidator = await readScript("lockingValidator.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);
const lockingAddressDetails: L.AddressDetails = L.getAddressDetails(lockingAddress);

// generate NFT metadatum for participant

// create personalised metadata
function genImageParticipant(name:string) {
  return {
      name: genericMetadata.name+name,
      image: genericMetadata.image,
      description: genericMetadata.description
  }
}

async function participantNumberToMerkleData(name:string,pubkeyhash:string): Promise<string> {
  const plutusMetaData = L.Data.fromJson(genImageParticipant(name));
  const datumHash = await blake2bHash(L.Data.to(new L.Constr(0,[plutusMetaData])));
  return pubkeyhash+datumHash;
}
// nft datum
const metadataDatum = await participantNumberToMerkleData(particpantsName,pkh);

// merkle tree stuff
const dataUint = merkleData.map((x) => L.fromHex(x));
const merkleTree = new L.MerkleTree(dataUint);
const merkleRoot: Types.Hash = { hash: L.toHex(merkleTree.rootHash())};

// a test member and proof
// retrieve the index of the user in the list of merkle tree data.
const n = merkleData.indexOf(metadataDatum)
// the following function will fail if n is -1 (the provided pkh and name are not in the merkletree)
const merkleProof1 : Types.MerkleProof = merkleTree.getProof(dataUint[n]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

// setup parameters
const prefixNFT: Types.Prefix = L.toLabel(222);
const prefixRef: Types.Prefix = L.toLabel(100);
const addrAlwaysFail: Types.Address = {addressCredential: { ScriptCredential: [lockingAddressDetails.paymentCredential.hash] }, addressStakingCredential: null};

const parameters: Types.Parameters = {
    merkleRoot: merkleRoot,
    prefixNFT: prefixNFT,
    prefixRef: prefixRef,
    threadSymbol: policyIdFree,
    lockAddress: addrAlwaysFail
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

async function burn(): Promise<L.TxHash> {
    const userTkn: L.Unit = L.toUnit(policyIdNFT,pkh,222);
    const refTkn: L.Unit = L.toUnit(policyIdNFT,pkh,100);
    const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
    const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[refTkn] == 1n);

    if (ourUTxO && ourUTxO.length > 0) {
      const tx = await lucid
      .newTx()
      .collectFrom(ourUTxO, L.Data.void())
      .attachSpendingValidator(lockingValidator)
      .mintAssets({ [userTkn]: -1n, [refTkn]: -1n},  L.Data.to(new L.Constr(1,[pkh])))
      .attachMintingPolicy(mintingScriptNFT)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
    }
    else return "No UTxO's found that can be burned"


}
console.log(await burn())
