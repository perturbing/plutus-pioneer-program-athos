import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import metadata from "../data/generic-NFT-metadata.json" assert { type: "json" };
import participants from "./participants.json" assert { type: "json" };

const lucid = await L.Lucid.new(
    undefined,
    "Preview"
);

// First we generate the mint merkle tree data

// a function that for a particpants name, generates the personalised metadata
function genImageParticipant(name:string) {
    return {
        name: metadata.name+name,
        image: metadata.image,
        description: metadata.description
    }
}

// a function that generates a merkle tree entry for a participant (given pkh and name)
async function participantToMintMerkleTreeData(name:string,pubkeyhash:string): Promise<string> {
    const plutusMetaData = L.Data.fromJson(genImageParticipant(name));
    const datumHash = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(new L.Constr(0,[plutusMetaData])))));
    return pubkeyhash+datumHash;
}

// a funct that for maps over all participants.json entries and converts it to a array of the mint merkle tree data
async function generateMintMerkleTreeData(participants: JSON[]) {
    return await Promise.all(participants.map(async (p) => {
        const merkleString = await participantToMintMerkleTreeData(p.name,p.pubkeyhash);
        return merkleString
    }))
}

// init the data with the above function
const mintData = await generateMintMerkleTreeData(participants);
const mintDataString = `export default ${JSON.stringify(mintData)};`;
// write the data to the data folder.
Deno.writeTextFileSync("./data/mint-merkleTree-Data.ts", mintDataString);


// Generating the merkle tree for the start phase

// a function that can convert an integer into a Uint8Array (Big endian) (this is i2osp)
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

// a function that generates a merkle tree entry for a participant (given pkh, id number, script cbor)
async function participantToStartMerkleTreeData(pubkeyhash:string,id:number,cbor:string): Promise<string> {
    const datumHash = L.toHex(L.C.hash_blake2b256(L.fromHex(L.Data.to(pubkeyhash))));
    const examValidator: L.SpendingValidator = {
        type: "PlutusV2",
        script: cbor
    };
    const scriptHash: L.scriptHash =  lucid.utils.validatorToScriptHash(examValidator);

    return pubkeyhash+L.toHex(intToUint8Array(id))+scriptHash+datumHash;
}

// a funct that for maps over all participants.json entries and converts it to a array of state merkle tree data
async function generateStartMerkleTreeData(participants: JSON[]) {
    return await Promise.all(participants.map(async (p) => {
        const merkleString = await participantToStartMerkleTreeData(p.pubkeyhash,p.id,p.examCBOR);
        return merkleString
    }))
}

// init the data with the above function
const startData = await generateStartMerkleTreeData(participants);
const startDataString = `export default ${JSON.stringify(startData)};`;
// write the data to the data folder.
Deno.writeTextFileSync("./data/start-merkleTree-Data.ts", startDataString);
