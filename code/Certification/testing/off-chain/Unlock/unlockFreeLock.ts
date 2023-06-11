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
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// the always true validator where the the test participants lock their thread token
const scriptCBOR = "49480100002221200101"
const lockingValidatorParticipant: L.SpendingValidator = {
  type: "PlutusV2",
  script: scriptCBOR
}
const lockingAddressParticipant: L.Address = lucid.utils.validatorToAddress(lockingValidatorParticipant);


const threadScript: L.MintingPolicy = setupData.threadScript;
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);

async function mint(): Promise<L.TxHash> {
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddressParticipant);
  const threadTkn: L.Unit = L.toUnit(threadPol,pkh);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[threadTkn] == 1n);

  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(0,[])))
    .attachSpendingValidator(lockingValidatorParticipant)
    .addSignerKey(pkh)
    .complete({ nativeUplc: false });
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
  }
  else return "No UTxO's found that can be burned"
}
console.log(await mint())