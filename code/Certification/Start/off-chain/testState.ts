import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "./types.ts"
import { secretSeed } from "./seed.ts";
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

// import always true minting policy (replace for threadtoken policy)
const mintingScriptFree: L.MintingPolicy = await readScript("alwaysTrue-policy.plutus");
const policyIdFree: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptFree);
console.log(policyIdFree)

// import NFT minting policy and apply above parameters
const Params = L.Data.Tuple([Types.CurrencySymbol]);
type Params = L.Data.Static<typeof Params>;
const currencySymbol: Types.CurrencySymbol = policyIdFree;
const params: Params = [currencySymbol];

async function readStateScript(): Promise<L.SpendingValidator> {
  const script = JSON.parse(await Deno.readTextFile("assets/start-state-validator.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<Params>(script.cborHex,params,Params)
  }
}
const stateScript: L.SpendingValidator = await readStateScript();
const lockingAddress: L.Address = lucid.utils.validatorToAddress(stateScript);
console.log(lockingAddress)

async function initState(): Promise<L.TxHash> {
  const tkn: L.Unit = L.toUnit(policyIdFree, L.fromText("State Token"));
  const tx = await lucid
    .newTx()
    .mintAssets({ [tkn]: 1n}, L.Data.void())
    .payToContract(lockingAddress, L.Data.to("00"),{[tkn]: 1n})
    .attachMintingPolicy(mintingScriptFree)
    .complete();
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
}

async function updateState(newState:string,bitNumber:bigint) {
  const tkn: L.Unit = L.toUnit(policyIdFree, L.fromText("State Token"));
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[tkn] == 1n);

  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(0,[bitNumber])))
    .attachSpendingValidator(stateScript)
    .payToContract(lockingAddress, L.Data.to(newState),{[tkn]: 1n})
    .complete({nativeUplc: false});
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
  }
  else return "No UTxO's found that can be burned"
}

async function unlockState() {
  const tkn: L.Unit = L.toUnit(policyIdFree, L.fromText("State Token"));
  const utxoAtScript: L.UTxO[] = await lucid.utxosAt(lockingAddress);
  const ourUTxO: L.UTxO[] = utxoAtScript.filter((utxo) => utxo.assets[tkn] == 1n);

  if (ourUTxO && ourUTxO.length > 0) {
    const tx = await lucid
    .newTx()
    .collectFrom(ourUTxO, L.Data.to(new L.Constr(1,[])))
    .attachSpendingValidator(stateScript)
    .complete({nativeUplc: false});
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
  }
  else return "No UTxO's found that can be burned"
}

// use the initState() function to create a token that marks the state and set the state to "00" (00000000 in bits).
//console.log(await initState());

// use the updateState() function to update the state by flipping one unset bit.
//console.log(await updateState("BF",5n))

// use the unlockState() for when the state reaches "FF" (or 11111111 in bits)
console.log(await unlockState())