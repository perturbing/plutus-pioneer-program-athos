import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import { secretSeed } from "../seed.ts";
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
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// import the locking validator (this locks the reference token)
const lockingValidator: L.SpendingValidator = await readScript("lockingValidator.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);

const mintingScriptNFT: L.MintingPolicy = setupData.mintingScriptNFT;
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