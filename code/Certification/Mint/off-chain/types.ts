import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";

// The type of a hash with the constraint that it must be of the exact size of 32 bytes
export const Hash = L.Data.Object({
    hash: L.Data.Bytes({ minLength: 32, maxLength: 32 }),
});
export type Hash = L.Data.Static<typeof Hash>;

// The type of a MT.proof from the Hydra merkle tree lib
export const MerkleProof = L.Data.Array(L.Data.Enum([
    L.Data.Object({ Left: L.Data.Tuple([Hash]) }),
    L.Data.Object({ Right: L.Data.Tuple([Hash]) })
]));
export type MerkleProof = L.Data.Static<typeof MerkleProof>;

// The type of a CurrencySymbol with the constraint that it must be of the exact size of 28 bytes
export const CurrencySymbol = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type CurrencySymbol = L.Data.Static<typeof CurrencySymbol>;

// The type of a prefix of CIP 67 with the constraint that it must be of the exact size of 4 bytes
export const Prefix = L.Data.Bytes({ minLength: 4, maxLength: 4 });
export type Prefix = L.Data.Static<typeof Prefix>;

export const PubKeyHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type PubKeyHash = L.Data.Static<typeof PubKeyHash>;

export const ValidatorHash = L.Data.Bytes({ minLength: 28, maxLength: 28 });
export type ValidatorHash = L.Data.Static<typeof ValidatorHash>;

export const Credential = L.Data.Enum([
    L.Data.Object({ PubKeyCredential: L.Data.Tuple([PubKeyHash]) }),
    L.Data.Object({ ScriptCredential: L.Data.Tuple([ValidatorHash]) })
]);
export type Credential = L.Data.Static<typeof Credential>;

//In an address, a chain pointer refers to a point of the chain containing a stake key registration certificate. A point is identified by 3 coordinates:
// An absolute slot number
// A transaction index (within that slot)
// A (delegation) certificate index (within that transaction)
export const StakingPtr = L.Data.Object({
    slotNumber: L.Data.Integer(),
    transactionIndex: L.Data.Integer(),
    certificateIndex: L.Data.Integer()
});
export type StakingPtr = L.Data.Static<typeof StakingPtr>;

export const StakingCredential = L.Data.Enum([
    L.Data.Object({ StakingHash: L.Data.Tuple([Credential]) }),
    L.Data.Object({ StakingPtr: L.Data.Tuple([StakingPtr]) })
]);
export type StakingCredential = L.Data.Static<typeof StakingCredential>;

export const Address = L.Data.Object({
    addressCredential: Credential,
    addressStakingCredential: L.Data.Nullable(StakingCredential)
});
export type Address = L.Data.Static<typeof Address>;

// The type of the Parameters for the NFT policy
export const Parameters = L.Data.Object({
    merkleRoot: Hash,
    prefixNFT: Prefix,
    prefixRef: Prefix,
    threadSymbol: CurrencySymbol,
    lockAddress: Address
});
export type Parameters = L.Data.Static<typeof Parameters>;