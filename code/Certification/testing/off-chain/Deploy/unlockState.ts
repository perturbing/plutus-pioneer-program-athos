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

const stateValidator: L.SpendingValidator = setupData.stateValidator;
const stateAddress: L.Address = lucid.utils.validatorToAddress(stateValidator);
console.log("State addr "+ stateAddress)

async function unlock(): Promise<L.TxHash> {
  const tkn: L.Unit = setupData.stateToken;
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(stateAddress);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[tkn] == 1n);
  
  const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(1,[])))
    .attachSpendingValidator(stateValidator)
    .mintAssets({ [tkn]: -1n}, L.Data.void())
    .attachMintingPolicy(mintingScriptFree)
    .complete();
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
}

console.log(await unlock())