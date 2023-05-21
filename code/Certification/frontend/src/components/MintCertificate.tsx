import {
    Address,
    AddressDetails,
    Constr,
    PolicyId,
    Unit,
    getAddressDetails,
    toLabel,
    toUnit,
} from "lucid-cardano";
import React, { useContext, useEffect, useState } from "react";
import { applyParamsToScript, Data, MintingPolicy } from "lucid-cardano";
import { AppStateContext } from "@/pages/_app";
import {
    signAndSubmitTx,
    genPersolanizedMetadata,
    genMerkleRoot,
    genMerkleProof,
} from "@/utilities/utilities";
import {
    CERTIFICATE_NFT_SCRIPT,
    GENERIC_METADATA,
    THREAD_TOKEN_MP,
    TOKEN_LOCK_VAL,
} from "@/utilities/constants";
import { Addr, CertifParameters, MerkleProof } from "@/utilities/types";

export default function MintCertificate() {
    const { appState, setAppState } = useContext(AppStateContext);
    const { lucid, wAddr } = appState;
    const [name, setName] = useState<string>("");
    const [metadata, setMetadata] = useState(GENERIC_METADATA);

    useEffect(() => {
        setMetadata(genPersolanizedMetadata(name));
    }, [name]);

    const getFinalCertificateMP = (): {
        lockingValAddress: Address;
        certNftMP: MintingPolicy;
    } => {
        const lockingValAddress: Address =
            lucid!.utils.validatorToAddress(TOKEN_LOCK_VAL);
        const lockingAddressDetails: AddressDetails =
            getAddressDetails(lockingValAddress);
        const addrAlwaysFail: Addr = {
            addressCredential: {
                ScriptCredential: [
                    lockingAddressDetails.paymentCredential!.hash,
                ],
            },
            addressStakingCredential: null,
        };

        const parameters: CertifParameters = {
            merkleRoot: genMerkleRoot(),
            prefixNFT: toLabel(222),
            prefixRef: toLabel(100),
            threadSymbol: lucid!.utils.mintingPolicyToId(THREAD_TOKEN_MP),
            lockAddress: addrAlwaysFail,
        };

        const Params = Data.Tuple([CertifParameters]);
        type Params = Data.Static<typeof Params>;
        const certNftMP: MintingPolicy = {
            type: "PlutusV2",
            script: applyParamsToScript<Params>(
                CERTIFICATE_NFT_SCRIPT,
                [parameters],
                Params
            ),
        };
        return { lockingValAddress, certNftMP };
    };

    const mintCertNFT = async () => {
        console.log("minting NFT for " + wAddr);
        if (wAddr && lucid) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            // Thread token
            const threadTknPolicyID: PolicyId =
                lucid.utils.mintingPolicyToId(THREAD_TOKEN_MP);
            const threadTkn: Unit = toUnit(threadTknPolicyID, pkh);

            // Certificate
            const { lockingValAddress, certNftMP } =
                await getFinalCertificateMP();
            const certNftPolicyID: PolicyId =
                lucid.utils.mintingPolicyToId(certNftMP);
            const userTkn: Unit = toUnit(certNftPolicyID, pkh, 222);
            const refTkn: Unit = toUnit(certNftPolicyID, pkh, 100);

            // Redeemer
            const Redeemer = Data.Object({ proof: MerkleProof });
            type Redeemer = Data.Static<typeof Redeemer>;
            const redeemer: Redeemer = {
                proof: await genMerkleProof(name, pkh),
            };

            // Transaction
            const tx = await lucid!
                .newTx()
                .mintAssets(
                    { [userTkn]: 1n, [refTkn]: 1n },
                    Data.to<Redeemer>(redeemer, Redeemer)
                )
                .attachMintingPolicy(certNftMP)
                .mintAssets({ [threadTkn]: -1n }, Data.void())
                .attachMintingPolicy(THREAD_TOKEN_MP)
                .payToContract(
                    lockingValAddress,
                    Data.to(new Constr(0, [Data.fromJson(metadata)])),
                    { [refTkn]: 1n }
                )
                .addSignerKey(pkh)
                .complete();

            await signAndSubmitTx(tx);
        }
    };

    async function mintThreadToken() {
        if (wAddr && lucid) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            // Thread token
            const threadTknPolicyID: PolicyId =
                lucid.utils.mintingPolicyToId(THREAD_TOKEN_MP);
            const threadTkn: Unit = toUnit(threadTknPolicyID, pkh);

            // Transaction
            const tx = await lucid
                .newTx()
                .mintAssets({ [threadTkn]: 1n }, Data.void())
                .attachMintingPolicy(THREAD_TOKEN_MP)
                .complete();

            await signAndSubmitTx(tx);
        }
    }

    return (
        <div>
            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p>Participant&apos;s name as shared with IOG:</p>
                <input
                    type="text"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    className=" w-40 py-1 px-2 ml-2 border border-zinc-700 rounded"
                />
            </div>

            <button
                onClick={mintThreadToken}
                disabled={!wAddr}
                className=" bg-zinc-800 text-white font-quicksand text-lg font-bold py-3 px-8 mr-5 rounded-lg shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600"
            >
                {" "}
                Mint Thread Toekn
            </button>

            <button
                onClick={mintCertNFT}
                disabled={!wAddr}
                className=" bg-zinc-800 text-white font-quicksand text-lg font-bold py-3 px-8 rounded-lg shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600"
            >
                {" "}
                Mint Certificate NFT
            </button>
        </div>
    );
}
