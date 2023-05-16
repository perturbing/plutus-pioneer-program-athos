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

async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("Mint/assets/"+ name))
    return {
      type: "PlutusV2",
      script: validator.cborHex
    }
}

// import always true minting policy (replace for threadtoken policy)
const mintingScriptFree: L.MintingPolicy = await readScript("free.plutus");
const policyIdFree: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptFree);

// import always fail validator (this locks the reference token)
const validatorAlwaysFail: L.SpendingValidator = await readScript("AlwaysFail.plutus");
const addressAlwaysFail: L.Address = lucid.utils.validatorToAddress(validatorAlwaysFail);
const details: L.AddressDetails = L.getAddressDetails(addressAlwaysFail);

// setup parameters
const root: Types.Hash = {hash: "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"};
const prefix1: Types.Prefix = L.toLabel(222);
const prefix2: Types.Prefix = L.toLabel(100);
const addrAlwaysFail: Types.Address = {addressCredential: { ScriptCredential: [details.paymentCredential.hash] }, addressStakingCredential: null};

const parameters: Types.Parameters = {
    merkleRoot: root,
    prefix1: prefix1,
    prefix2: prefix1,
    threadSymbol: policyIdFree,
    lockAddress: addrAlwaysFail
}

// import NFT minting policy and apply above parameters
const Params = L.Data.Tuple([Types.Parameters]);
type Params = L.Data.Static<typeof Params>;
async function readNFTPolicy(): Promise<L.MintingPolicy> {
  const script = JSON.parse(await Deno.readTextFile("Mint/assets/nft.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<Params>(script.cborHex,[parameters],Params)
  }
}
const mintingScriptNFT: L.MintingPolicy = await readNFTPolicy();
const policyIdNFT: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptNFT);
console.log(policyIdNFT)

async function mint(name: string): Promise<L.TxHash> {
    const tkn: L.Unit = L.toUnit(policyIdNFT,L.fromText(name),222);
    const tx = await lucid
      .newTx()
      .mintAssets({ [tkn]: 1n}, L.Data.to<Types.Hash>(root,Types.Hash))
      .attachMintingPolicy(mintingScriptNFT)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}
//console.log(await mint("test"))