import * as L from "https://deno.land/x/lucid@0.10.1/mod.ts";
import * as Types from "../types.ts";
import startMerkleData from "../../../frontend/src/data/start-merkleTree-Data.ts";
import mintMerkleData from "../../../frontend/src/data/mint-merkleTree-Data.ts";

// set blockfrost endpoint
const lucid = await L.Lucid.new(undefined, "Preview");

const TxID = L.Data.Bytes();
type TxID = L.Data.Static<typeof TxID>;

const Ix = L.Data.Integer();
type Ix = L.Data.Static<typeof Ix>;

const TokenName = L.Data.Bytes();
type TokenName = L.Data.Static<typeof TokenName>;

const txID: TxID =
    "1433724c96a5290e4c868eabcc242889a37fcda5022f330f71eebd3c89164df0";
const ix: Ix = 1n;
const tokenName: TokenName = L.fromText("PPP Cert State token");

// import NFT minting policy and apply above parameters
const NFTParams = L.Data.Tuple([TxID, Ix, TokenName]);
type NFTParams = L.Data.Static<typeof Params>;
const nftParameters: NFTParams = [txID, ix, tokenName];
async function readStateNFTPolicy(): Promise<L.MintingPolicy> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/state-nft-policy.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<NFTParams>(
            script.cborHex,
            nftParameters,
            NFTParams
        ),
    };
}
const mintingStateNFTScript: L.MintingPolicy = await readStateNFTPolicy();
const policyStateNFT: L.PolicyID = lucid.utils.mintingPolicyToId(
    mintingStateNFTScript
);
console.log("State NFT policyID: " + policyStateNFT);

// start merkle tree stuff
const startDataUint = startMerkleData.map((x) => L.fromHex(x));
const startMerkleTree = new L.MerkleTree(startDataUint);
const startMerkleRoot: Types.Hash = {
    hash: L.toHex(startMerkleTree.rootHash()),
};

// setup the thread parameter from the above merkle tree
const threadParameters: Types.ThreadParameters = {
    merkleRoot: startMerkleRoot,
    stateSymbol: policyStateNFT,
};

// import Thread minting policy and apply above parameters
const ThreadParams = L.Data.Tuple([Types.ThreadParameters]);
type ThreadParams = L.Data.Static<typeof ThreadParams>;
async function readThreadPol(): Promise<L.MintingPolicy> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/start-policy.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<ThreadParams>(
            script.cborHex,
            [threadParameters],
            ThreadParams
        ),
    };
}
const threadScript: L.MintingPolicy = await readThreadPol();
const threadPol: L.PolicyId = lucid.utils.mintingPolicyToId(threadScript);
console.log("Thread policyID: " + threadPol);

// setup the state parameter from the thread minting policy and final state
const stateParameters: Types.StateParameters = {
    threadSymbol: threadPol,
    finalState: "ff".repeat(1001),
};

const StateParams = L.Data.Tuple([Types.StateParameters]);
type StateParams = L.Data.Static<typeof StateParams>;
async function readStateVal(): Promise<L.SpendingValidator> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/start-state-validator.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<StateParams>(
            script.cborHex,
            [stateParameters],
            StateParams
        ),
    };
}
const stateValidator: L.SpendingValidator = await readStateVal();
const stateAddress: L.Address = lucid.utils.validatorToAddress(stateValidator);
console.log("State addr " + stateAddress);

// a helper function that reads an unparametrized plutus script
async function readScript(name: string): Promise<L.MintingPolicy> {
    const validator = JSON.parse(await Deno.readTextFile("assets/" + name));
    return {
        type: "PlutusV2",
        script: validator.cborHex,
    };
}

// import the locking validator (this locks the reference token of the PPP Certificate NFT)
const lockingValidator: L.SpendingValidator = await readScript(
    "lockingValidator.plutus"
);
const lockingAddress: L.Address =
    lucid.utils.validatorToAddress(lockingValidator);
const lockingAddressDetails: L.AddressDetails =
    L.getAddressDetails(lockingAddress);

// mint merkle tree stuff
const mintDataUint = mintMerkleData.map((x) => L.fromHex(x));
const mintMerkleTree = new L.MerkleTree(mintDataUint);
const mintMerkleRoot: Types.Hash = { hash: L.toHex(mintMerkleTree.rootHash()) };

// setup parameters
const prefixNFT: Types.Prefix = L.toLabel(222);
const prefixRef: Types.Prefix = L.toLabel(100);
const lockAddr: Types.Address = {
    addressCredential: {
        ScriptCredential: [lockingAddressDetails.paymentCredential.hash],
    },
    addressStakingCredential: null,
};

const parameters: Types.Parameters = {
    merkleRoot: mintMerkleRoot,
    prefixNFT: prefixNFT,
    prefixRef: prefixRef,
    threadSymbol: threadPol,
    lockAddress: lockAddr,
};

// import NFT minting policy and apply above parameters
const Params = L.Data.Tuple([Types.Parameters]);
type Params = L.Data.Static<typeof Params>;
async function readNFTPolicy(): Promise<L.MintingPolicy> {
    const script = JSON.parse(
        await Deno.readTextFile("assets/certificate-policy.plutus")
    );
    return {
        type: "PlutusV2",
        script: L.applyParamsToScript<Params>(
            script.cborHex,
            [parameters],
            Params
        ),
    };
}
const mintingScriptNFT: L.MintingPolicy = await readNFTPolicy();
const policyIdNFT: L.PolicyId = lucid.utils.mintingPolicyToId(mintingScriptNFT);
console.log("NFT PolicyID: " + policyIdNFT);

const setup = {
    txId: txID,
    iX: Number(ix),
    stateScript: mintingStateNFTScript,
    stateToken: L.toUnit(policyStateNFT, tokenName),
    threadScript: threadScript,
    stateValidator: stateValidator,
    mintingScriptNFT: mintingScriptNFT,
    lockingValidator: lockingValidator,
};

const setupData = `export default ${JSON.stringify(setup)};`;
// write the data to the data folder.
Deno.writeTextFileSync("frontend/src/data/setupData.ts", setupData);
