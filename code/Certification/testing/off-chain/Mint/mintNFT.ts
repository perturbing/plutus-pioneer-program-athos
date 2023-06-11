import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts";
import { secretSeed } from "../seed.ts";
import genericMetadata from "../../../data/generic-NFT-metadata.json" assert { type: "json" };
import merkleData from "../../../data/mint-merkleTree-Data.ts";
import setupData from "../../../data/setupData.ts";

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
const particpantsName: string = "Name0"
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

const threadScript: L.MintingPolicy = setupData.threadScript;
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);

// import the locking validator (this locks the reference token)
const lockingValidator: L.SpendingValidator = await readScript("lockingValidator.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);

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
  const datumHash = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[plutusMetaData])))))
  return pubkeyhash+datumHash;
}
// nft datum
const metadataDatum = await participantNumberToMerkleData(particpantsName,pkh);

// merkle tree stuff
const mintDataUint = merkleData.map((x) => L.fromHex(x));
const mintMerkleTree = new L.MerkleTree(mintDataUint);

// a test member and proof
// retrieve the index of the user in the list of merkle tree data.
const n = merkleData.indexOf(metadataDatum)
// the following function will fail if n is -1 (the provided pkh and name are not in the merkletree)
const merkleProof1 : Types.MerkleProof = mintMerkleTree.getProof(mintDataUint[n]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

const mintingScriptNFT: L.MintingPolicy = setupData.mintingScriptNFT;
const policyIdNFT: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptNFT);
console.log("PolicyID: "+policyIdNFT)

const Redeemer = L.Data.Object({proof: Types.MerkleProof})
type Redeemer = L.Data.Static<typeof Redeemer>

const redeemer: Redeemer = {proof: merkleProof1}

async function mint(): Promise<L.TxHash> {
    const threadTkn: L.Unit = L.toUnit(threadPol,pkh);
    const userTkn: L.Unit = L.toUnit(policyIdNFT,pkh,222);
    const refTkn: L.Unit = L.toUnit(policyIdNFT,pkh,100)
    const tx = await lucid
      .newTx()
      .mintAssets({ [userTkn]: 1n, [refTkn]: 1n},  L.Data.to<Redeemer>(redeemer,Redeemer))
      .attachMintingPolicy(mintingScriptNFT)
      .mintAssets({ [threadTkn]:-1n }, L.Data.to(new L.Constr(1,[])))
      .attachMintingPolicy(threadScript)
      .payToContract(lockingAddress, L.Data.to(new L.Constr(0,[L.Data.fromJson(genImageParticipant(particpantsName))])),{[refTkn]: 1n})
      .addSignerKey(pkh)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}
console.log(await mint())