import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts"
import { secretSeed } from "../seed.ts";
import merkleData from "../../../data/start-merkleTree-Data.ts";
import setupData from "../../../data/setupData.ts";

function setBit(hexStr: string, bitIndex: number): string {
  const newStr = ((BigInt(1) << BigInt(bitIndex)) | BigInt("0x"+hexStr)).toString(16);
  return newStr.padStart(hexStr.length,"0")
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
const participantId: number = 1;
const scriptCBOR = "49480100002221200101"
// the validator where the participant should lock their thread token
const lockingValidatorParticipant: L.SpendingValidator = {
  type: "PlutusV2",
  script: scriptCBOR
}
const lockingAddressParticipant: L.Address = lucid.utils.validatorToAddress(lockingValidatorParticipant);


// start merkle tree stuff
const startDataUint = merkleData.map((x) => L.fromHex(x));
const startMerkleTree = new L.MerkleTree(startDataUint);

// get for above user a merkle tree proof
const merkleProof : Types.MerkleProof = startMerkleTree.getProof(startDataUint[participantId]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

const threadScript: L.MintingPolicy = setupData.threadScript;
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);

const stateValidator: L.SpendingValidator = setupData.stateValidator;
const stateAddress: L.Address = lucid.utils.validatorToAddress(stateValidator);

const Redeemer = L.Data.Object({
  proof: Types.MerkleProof,
  pkh:   Types.PubKeyHash,
  n:     L.Data.Integer()
})
type Redeemer = L.Data.Static<typeof Redeemer>

const redeemer: Redeemer = {proof: merkleProof, pkh: pkh, n: BigInt(participantId)}
async function mint(): Promise<L.TxHash> {
  const stateTkn: L.Unit = setupData.stateToken;
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(stateAddress);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[stateTkn] == 1n);
  const threadTkn: L.Unit = L.toUnit(threadPol,pkh);

  if (ourUTxO && ourUTxO.length > 0) {
    const stateDatum = await lucid.provider.getDatum(ourUTxO[0].datumHash)
    console.log("old state: "+L.Data.from(stateDatum))
    const newState = setBit(L.Data.from(stateDatum),participantId)
    console.log("new state: "+ newState)

    const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(0,[])))
    .attachSpendingValidator(stateValidator)
    .payToContract(stateAddress, L.Data.to(newState),{[stateTkn]: 1n})
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