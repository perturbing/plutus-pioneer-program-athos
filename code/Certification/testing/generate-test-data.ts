import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import metadata from "../Data/generic_metadata.json" assert { type: "json" };
import participants from "./participants.json" assert { type: "json" };
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

function genImageParticipant(name:string) {
    return {
        name: metadata.name+name,
        image: metadata.image,
        description: metadata.description
    }
}

async function participantNumberToMerkleData(name:string,pubkeyhash:string): Promise<string> {
    const plutusMetaData = L.Data.fromJson(genImageParticipant(name));
    const datumHash = await blake2bHash(L.Data.to(new L.Constr(0,[plutusMetaData])));
    return pubkeyhash+datumHash;
}

async function generateAllParticipantData(participants: JSON[]) {
    return await Promise.all(participants.map(async (p) => {
        const merkleString = await participantNumberToMerkleData(p.name,p.pubkeyhash);
        return merkleString
    }))
}

const data = await generateAllParticipantData(participants);
const dataString = `export default ${JSON.stringify(data)};`;
Deno.writeTextFileSync("./Data/merkleData.ts", dataString);