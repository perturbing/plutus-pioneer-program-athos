import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
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

// merkle tree stuff
const Hash = L.Data.Object({
  hash: L.Data.Bytes({ minLength: 32, maxLength: 32 }),
});
type Hash = L.Data.Static<typeof Hash>;

const MerkleProof = L.Data.Array(L.Data.Enum([
  L.Data.Object({ Left: L.Data.Tuple([Hash]) }),
  L.Data.Object({ Right: L.Data.Tuple([Hash]) }),
]));
type MerkleProof = L.Data.Static<typeof MerkleProof>;

const data = ["first object", "second object", "third object","forth object"]
const dataUint = data.map( (a) => L.fromHex(L.fromText(a)));
const merkleTree = new L.MerkleTree(dataUint);
const merkleRoot: Hash = { hash: L.toHex(merkleTree.rootHash())};

// a test member and proof
const n = 3;
const member = L.fromText(data[n])
const merkleProof1 : MerkleProof = merkleTree.getProof(dataUint[n]).map((p) =>
  p.left
    ? { Left: [{ hash: L.toHex(p.left) }] }
    : { Right: [{ hash: L.toHex(p.right!) }] }
)

// Policy stuff
const Params = L.Data.Tuple([Hash]);
type Params = L.Data.Static<typeof Params>;
async function readPolicy(): Promise<L.MintingPolicy> {
  const validator = JSON.parse(await Deno.readTextFile("Mint/assets/policy.plutus"))
  return {
    type: "PlutusV2",
    script: L.applyParamsToScript<Params>(validator.cborHex,[merkleRoot],Params)
  }
}

const mintingScript: L.MintingPolicy = await readPolicy();
const policyId: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScript);

const Redeemer = L.Data.Object({
  member: L.Data.Bytes(),
  merkleProof: MerkleProof
})
type Redeemer = L.Data.Static<typeof Redeemer>

async function mint(name: string): Promise<L.TxHash> {
  const tkn: L.Unit = L.toUnit(policyId,L.fromText(name),222);
  const tx = await lucid
    .newTx()
    .mintAssets({ [tkn]: 1n}, L.Data.to<Redeemer>({member:member,merkleProof:merkleProof1},Redeemer))
    .attachMintingPolicy(mintingScript)
    .complete();
  const signedTx = await tx.sign().complete();
 
  return signedTx.submit();
}

console.log(await mint(""));



