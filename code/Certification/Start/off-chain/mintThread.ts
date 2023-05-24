import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "./types.ts"
import { secretSeed } from "./seed.ts";
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
const pkh: string = L.getAddressDetails(addr).paymentCredential.hash;
const participantNumber: number = 1;
// add this functionality later
//const lockScript: L.SpendingValidator = {
//  type: "PlutusV2",
//  script: "59098159097e01000032332232323322323232323232323232323232332232323232323232323232323222232325335323232333355300d1200133500e22230033002001200122533500210011029335530091200123500122233355301112001323350132233350032200200200135001220011233001225335002102f100102c23500122253353302e007003153353302e006002132333573466e1c0040080c40c0cdc0a400000a205e205e601c6aa00a444444444444010601460446aa66a666aa601a2400266a01c44a66a004420062002a02a4a66a6a0024464a66a666ae68cdc79a800910011a801910010160158999ab9a3370e6a002440026a0064400205805620566a00a446666a0024c4c4c400226a02e0022a02c6aa002444444444444018426a002440022c444400605026a002440046666ae68cdc39aab9d5003480008cc8848cc00400c008c8c8c8c8c8c8c8c8c8c8c8c8c8cccd5cd19b8735573aa018900011999999999999111111111110919999999999980080680600580500480400380300280200180119a80e00e9aba1500c33501c01d35742a01666a03803c6ae854028ccd54081d7280f9aba150093335502075ca03e6ae854020cd407009cd5d0a803999aa8100143ad35742a00c6464646666ae68cdc39aab9d5002480008cc8848cc00400c008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a8193ad35742a00460666ae84d5d1280111931901a99ab9c036035033135573ca00226ea8004d5d0a8011919191999ab9a3370e6aae754009200023322123300100300233503275a6ae854008c0ccd5d09aba2500223263203533573806c06a06626aae7940044dd50009aba135744a004464c6406266ae700c80c40bc4d55cf280089baa00135742a00a66a038eb8d5d0a802199aa81001210009aba150033335502075c40026ae854008c098d5d09aba2500223263202d33573805c05a05626ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aab9e5001137540026ae85400cc058d5d09aba2500323263201f33573804003e03a6666ae68cdc39aab9d37540089000100f11931900f19ab9c01f01e01c101d13263201d335738921035054350001d135573ca00226ea8004c8004d540808844894cd40045407c884cd4080c010008cd54c018480040100048cc009406c004c8004d540788894cd40044008884d400888cc01cccc02000801800400cc8004d5407488894cd40044008884d4008894cd4ccd5cd19b870014800009008c4ccc02001c01800c4ccc02001ccd407c48ccc00402000c00801800cc8004d5407088448894cd40044d400c88004884ccd401488008c010008ccd54c01c4800401401000448848cc00400c00848c88c008dd6000990009aa80d911999aab9f00125019233501830043574200460066ae880080508c8c8cccd5cd19b8735573aa004900011991091980080180118061aba150023005357426ae8940088c98c8050cd5ce00a80a00909aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa0049000119910919800801801180a9aba1500233500d014357426ae8940088c98c8064cd5ce00d00c80b89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6403666ae7007006c06406005c4d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201533573802c02a02626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355018223233335573e0044a02e466a02c66aa030600c6aae754008c014d55cf280118021aba200301213574200224464646666ae68cdc3a800a400046a00e600a6ae84d55cf280191999ab9a3370ea00490011280391931900919ab9c01301201000f135573aa00226ea800448488c00800c44880048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900819ab9c01101000e00d00c00b135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900619ab9c00d00c00a135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8028cd5ce00580500409baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c804ccd5ce00a00980880800780700680600589aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6401866ae700340300280244d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263200933573801401200e00c26aae7540044dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401466ae7002c02802001c0184d55cea80089baa0012323333573466e1d40052002200d23333573466e1d40092000200d23263200633573800e00c00800626aae74dd5000a4c2400292103505431003200135500722533500115004221350022253353300a0024890010031335007335500900200130060031122002122122330010040031122123300100300222333573466e3c00800401000c488008488004448c8c00400488cc00cc0080080041",
//};
//const lockDatum:

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
  const validator = JSON.parse(await Deno.readTextFile("assets/"+ name))
  return {
    type: "PlutusV2",
    script: validator.cborHex
  }
}

const lockingValidator: L.SpendingValidator = await readScript("lockingValidator.plutus");
const lockingAddress: L.Address = lucid.utils.validatorToAddress(lockingValidator);
const lockingAddressDetails: L.AddressDetails = L.getAddressDetails(lockingAddress);
const lockingAddressScriptHash = lockingAddressDetails.paymentCredential.hash;
const lockingAddressDatumHash = await blake2bHash(L.Data.to(new L.Constr(0,[])))


// import always true minting policy (replace for threadtoken policy)
const mintingScriptFree: L.MintingPolicy = await readScript("alwaysTrue-policy.plutus");
const policyIdFree: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptFree);

function intToUint8Array(num: number): Uint8Array {
  if (num === 0) {
    return new Uint8Array([0]);
  }
  
  let byteCount = Math.ceil(Math.log2(num + 1) / 8); // calculate byte count
  
  let arr = new Uint8Array(byteCount);
  for(let i = byteCount - 1; i >= 0; i--) {
    arr[i] = num & 0xFF;
    num >>= 8;
  }

  return arr;
}
// merkle tree stuff
const data = [pkh+L.toHex(intToUint8Array(1))+lockingAddressScriptHash+lockingAddressDatumHash,pkh+L.toHex(intToUint8Array(22244))+lockingAddressScriptHash+lockingAddressDatumHash];
const dataUint = data.map( (a) => L.fromHex(a));
const merkleTree = new L.MerkleTree(dataUint);
const merkleRoot: Types.Hash = { hash: L.toHex(merkleTree.rootHash())};

// a test member and proof
const n = 1;
const member = L.fromText(data[n])
const merkleProof1 : Types.MerkleProof = merkleTree.getProof(dataUint[n]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

const parameters: Types.Parameters = {
  merkleRoot: merkleRoot,
  stateSymbol: policyIdFree,
}

// import NFT minting policy and apply above parameters
const Params = L.Data.Tuple([Types.Parameters]);
type Params = L.Data.Static<typeof Params>;
async function readNFTPolicy(): Promise<L.MintingPolicy> {
const script = JSON.parse(await Deno.readTextFile("assets/start-policy.plutus"))
return {
  type: "PlutusV2",
  script: L.applyParamsToScript<Params>(script.cborHex,[parameters],Params)
}
}
const mintingScriptThread: L.MintingPolicy = await readNFTPolicy();
const policyIdThread: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptThread);
console.log("PolicyID: "+policyIdThread)

// a test function for creating the state token
async function mintStateToken(): Promise<L.TxHash> {
  const tkn: L.Unit = L.toUnit(policyIdFree,L.fromText("stateToken"));
  const tx = await lucid
    .newTx()
    .mintAssets({ [tkn]: 1n}, L.Data.void())
    .attachMintingPolicy(mintingScriptFree)
    .complete();
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
}
//console.log(await mintPKHToken());

const Redeemer = L.Data.Object({
  proof: Types.MerkleProof,
  pkh:   Types.PubKeyHash,
  n:     L.Data.Integer()
})
type Redeemer = L.Data.Static<typeof Redeemer>

const redeemer: Redeemer = {proof: merkleProof1, pkh: pkh, n: 22244n}
//const redeemer: Redeemer = { n: 1n}

async function mint(): Promise<L.TxHash> {
    const threadTkn: L.Unit = L.toUnit(policyIdThread,pkh);
    const tx = await lucid
      .newTx()
      .mintAssets({ [threadTkn]: 1n }, L.Data.to<Redeemer>(redeemer,Redeemer))
      .attachMintingPolicy(mintingScriptThread)
      .payToContract(lockingAddress, L.Data.to(new L.Constr(0,[])),{[threadTkn]: 1n})
      .addSignerKey(pkh)
      .complete();
    const signedTx = await tx.sign().complete();
   
    return signedTx.submit();
}
console.log(await mint())