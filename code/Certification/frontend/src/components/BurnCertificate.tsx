import {
    Address,
    AddressDetails,
    Constr,
    PolicyId,
    SpendingValidator,
    UTxO,
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
} from "@/utilities/utilities";
import {
    CERTIFICATE_NFT_SCRIPT,
    GENERIC_METADATA,
    THREAD_TOKEN_MP,
    TOKEN_LOCK_VAL,
} from "@/utilities/constants";
import { Addr, CertifParameters, MerkleProof } from "@/utilities/types";
import setupData from "@/data/setupData";
import { TextContainer } from "./TextContainer";

// INFO: Use `testing/off-chain/Mint/burnNFT.ts` transaction

export default function BurnCertificate() {
    const { appState, setAppState } = useContext(AppStateContext);
    const { lucid, wAddr } = appState;
    const [name, setName] = useState<string>("");
    const [metadata, setMetadata] = useState(GENERIC_METADATA);

    useEffect(() => {
        setMetadata(genPersolanizedMetadata(name));
    }, [name]);

    const burnCertNFT = async () => {
        if (wAddr && lucid) {
            const pkh: string =
                getAddressDetails(wAddr).paymentCredential?.hash || "";

            // import the locking validator (this locks the reference token)
            const lockingValidator: SpendingValidator =
                setupData.lockingValidator as SpendingValidator;
            const lockingAddress: Address =
                lucid.utils.validatorToAddress(lockingValidator);

            const mintingScriptNFT: MintingPolicy =
                setupData.mintingScriptNFT as MintingPolicy;
            const policyIdNFT: PolicyId =
                lucid.utils.mintingPolicyToId(mintingScriptNFT);

            const userTkn: Unit = toUnit(policyIdNFT, pkh, 222);
            const refTkn: Unit = toUnit(policyIdNFT, pkh, 100);
            const utxoAtScript: UTxO[] = await lucid.utxosAt(lockingAddress);
            const ourUTxO: UTxO[] = utxoAtScript.filter(
                (utxo) => utxo.assets[refTkn] == 1n
            );

            if (ourUTxO && ourUTxO.length > 0) {
                const tx = await lucid
                    .newTx()
                    .collectFrom(ourUTxO, Data.void())
                    .attachSpendingValidator(lockingValidator)
                    .mintAssets(
                        { [userTkn]: -1n, [refTkn]: -1n },
                        Data.to(new Constr(1, [pkh]))
                    )
                    .attachMintingPolicy(mintingScriptNFT)
                    .complete();
                const signedTx = await tx.sign().complete();

                return signedTx.submit();
            } else alert("No UTxO's found that can be burned");
        }
    };

    return (
        <div className="flex flex-col">
            <TextContainer>
                <p>
                    Are you sure you want to burn your certificate?{" "}
                    <b>You won&apos;t be able to mint another one!</b>
                </p>
            </TextContainer>
            <button
                onClick={burnCertNFT}
                disabled={!wAddr}
                className=" bg-zinc-800 text-white font-quicksand text-lg font-bold py-3 px-8 rounded-lg shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600"
            >
                {" "}
                Burn Certificate NFT
            </button>
        </div>
    );
}
