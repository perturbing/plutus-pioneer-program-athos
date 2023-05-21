import {
    Constr,
    Data,
    Lucid,
    MerkleTree,
    TxComplete,
    TxHash,
    fromHex,
    toHex,
} from "lucid-cardano";
import { GENERIC_METADATA, MERKLE_DATA } from "./constants";
import { Hash32, MerkleProof } from "./types";

export const signAndSubmitTx = async (tx: TxComplete): Promise<TxHash> => {
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    console.log(`Transaction submitted: ${txHash}`);
    alert(`Transaction submitted: ${txHash}`);
    return txHash;
};

export const safeStringToBigInt = (r: string): bigint | undefined => {
    const parsed = BigInt(Number(r));
    if (Number.isNaN(parsed)) return;
    return parsed;
};

export const findUTxO = async (lucid: Lucid, ref: string) => {
    const [txH, ix] = ref.split("#");
    const utxos = await lucid.utxosByOutRef([
        {
            txHash: txH,
            outputIndex: Number(ix),
        },
    ]);
    return utxos[0];
};

// create personalised metadata
export function genPersolanizedMetadata(name: string) {
    return {
        name: GENERIC_METADATA.name + name,
        image: GENERIC_METADATA.image,
        description: GENERIC_METADATA.description,
    };
}

export function genMerkleRoot() {
    const dataUint = MERKLE_DATA.map((x) => fromHex(x));
    const merkleTree = new MerkleTree(dataUint);
    const merkleRoot: Hash32 = { hash: toHex(merkleTree.rootHash()) };
    return merkleRoot;
}

export async function participantNumberToMerkleData(
    name: string,
    pubkeyhash: string
): Promise<string> {
    const plutusMetaData = Data.fromJson(genPersolanizedMetadata(name));
    const datum = Data.to(new Constr(0, [plutusMetaData]));
    const datumHash = await (
        await fetch("api/hash", {
            method: "POST",
            headers: {
                "Content-type": "application/json",
            },
            body: JSON.stringify({ datum: datum }),
        })
    ).json();

    console.log("pkh+datumHash: " + pubkeyhash + datumHash.hash);

    return pubkeyhash + datumHash.hash;
}

export async function genMerkleProof(name: string, pkh: string) {
    const dataUint = MERKLE_DATA.map((x) => fromHex(x));
    const merkleTree = new MerkleTree(dataUint);

    // nft datum
    const metadataDatum = await participantNumberToMerkleData(name, pkh);
    // retrieve the index of the user in the list of merkle tree data.
    const n = MERKLE_DATA.indexOf(metadataDatum);
    console.log("index of th user in the list: " + n);

    const merkleProof: MerkleProof = merkleTree
        .getProof(dataUint[n])
        .map((p) =>
            p.left
                ? { Left: [{ hash: toHex(p.left) }] }
                : { Right: [{ hash: toHex(p.right!) }] }
        );

    return merkleProof;
}
