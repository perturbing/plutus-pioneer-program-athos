import {
    Address,
    Constr,
    PolicyId,
    SpendingValidator,
    TxComplete,
    UTxO,
    Unit,
    getAddressDetails,
    toUnit,
} from "lucid-cardano";
import React, { useContext, useState } from "react";
import { Data, MintingPolicy } from "lucid-cardano";
import { AppStateContext } from "@/pages/_app";
import {
    signAndSubmitTx,
    genStartMerkleProof,
    setBit,
} from "@/utilities/utilities";
import { MerkleProof, PubKeyHash } from "@/utilities/types";
import setupData from "@/data/setupData";
import { TextContainer } from "./TextContainer";
import { SignElswhere } from "./SignElswhere";

export default function StartExam() {
    const { appState, setAppState } = useContext(AppStateContext);
    const { lucid, wAddr } = appState;
    const [participantId, setParticipantId] = useState<number>(0);
    const [cbor, setCbor] = useState<string | null>(
        "59094159093e01000033232323233223232323232323232332232332232323232323232323232323232222232325335323232333355300b12001500f225335002100110263322330025335333573466e24cd54038c03448004d5400c88888888888800d2006026027102713357389201013200026330025335333573466e24cd54038c03448004d5400c8888888888880312006026027102713357389201013500026330025335533533355300d12001500c2533532333573466ebc0080040a40a0dd4a4410b48656c6c6f20576f726c640013501e0011501d33553013120012350012200135500322222222222200221028102610271335738921013600026001501750183235001222222222222533533355301812001501725335333573466e3c0440040cc0c84d40a4004540a0010840cc40c540044d400488008cccd5cd19b8735573aa0069000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233501f02035742a01866a03e0406ae85402ccd407c084d5d0a805199aa811bae502235742a012666aa046eb94088d5d0a80419a80f8161aba150073335502302d75a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233503775a6ae854008c0e0d5d09aba2500223263203c33573807a07807426aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a06eeb4d5d0a801181c1aba135744a004464c6407866ae700f40f00e84d55cf280089baa001357426ae8940088c98c80e0cd5ce01c81c01b09aab9e5001137540026ae854014cd407dd71aba1500433355023029200135742a006666aa046eb88004d5d0a80118159aba135744a004464c6406866ae700d40d00c84d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a801980d9aba135744a006464c6404c66ae7009c098090dd7003081209931901219ab9c49010350543500024135573ca00226ea80044cd4018894cd40088400c40054040c8004d5407888448894cd40044d401c00c884ccd4028014c010008ccd54c01c480040140100044488cccd54008c8cd401c88ccd401c00c004008d4010005401488cdc0000a40040029000091a80091000891a80091001099a8009111801980100090008910919800801801190009aa80c110891299a8008a80591099a806180200119aa9803090008020008919118011bac001320013550182233335573e0024a014466a01260086ae84008c00cd5d100100c119191999ab9a3370e6aae7540092000233221233001003002300e35742a004600a6ae84d5d1280111931900c19ab9c019018016135573ca00226ea80048c8c8c8c8cccd5cd19b8735573aa00890001199991110919998008028020018011919191999ab9a3370e6aae7540092000233221233001003002301735742a00466a01e02c6ae84d5d1280111931900e99ab9c01e01d01b135573ca00226ea8004d5d0a802199aa8043ae500735742a0066464646666ae68cdc3a800a4008464244460040086ae84d55cf280191999ab9a3370ea0049001119091118008021bae357426aae7940108cccd5cd19b875003480008488800c8c98c807ccd5ce01000f80e80e00d89aab9d5001137540026ae854008cd402dd71aba135744a004464c6403266ae7006806405c4d5d1280089aba25001135573ca00226ea80044cd54005d73ad112232230023756002640026aa02a44646666aae7c008940208cd401ccc8848cc00400c008c018d55cea80118029aab9e500230043574400602c26ae840044488008488488cc00401000c488c8c8cccd5cd19b875001480008d401cc014d5d09aab9e500323333573466e1d400920022500723263201433573802a02802402226aae7540044dd50008909118010018891000919191999ab9a3370ea002900311909111180200298039aba135573ca00646666ae68cdc3a8012400846424444600400a60126ae84d55cf280211999ab9a3370ea006900111909111180080298039aba135573ca00a46666ae68cdc3a8022400046424444600600a6eb8d5d09aab9e500623263201233573802602402001e01c01a26aae7540044dd5000919191999ab9a3370e6aae7540092000233221233001003002300535742a0046eb4d5d09aba2500223263200e33573801e01c01826aae7940044dd50009191999ab9a3370e6aae75400520002375c6ae84d55cf280111931900619ab9c00d00c00a13754002464646464646666ae68cdc3a800a401842444444400646666ae68cdc3a8012401442444444400846666ae68cdc3a801a40104664424444444660020120106eb8d5d0a8029bad357426ae8940148cccd5cd19b875004480188cc8848888888cc008024020dd71aba15007375c6ae84d5d1280391999ab9a3370ea00a900211991091111111980300480418061aba15009375c6ae84d5d1280491999ab9a3370ea00c900111909111111180380418069aba135573ca01646666ae68cdc3a803a400046424444444600a010601c6ae84d55cf280611931900a99ab9c01601501301201101000f00e00d135573aa00826aae79400c4d55cf280109aab9e5001137540024646464646666ae68cdc3a800a4004466644424466600200a0080066eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023212230020033008357426aae7940188c98c8038cd5ce00780700600589aab9d5003135744a00226aae7940044dd5000919191999ab9a3370ea002900111909118008019bae357426aae79400c8cccd5cd19b875002480008c8488c00800cdd71aba135573ca008464c6401666ae7003002c0240204d55cea80089baa00112232323333573466e1d400520042122200123333573466e1d40092002232122230030043006357426aae7940108cccd5cd19b87500348000848880088c98c8030cd5ce00680600500480409aab9d5001137540024646666ae68cdc3a800a4004400a46666ae68cdc3a80124000400a464c6401066ae700240200180144d55ce9baa001122002122001498480052410350543100112323001001223300330020020014c11e581ce48dea1ea0c60cfeed371d456419c10e83c72d772ec4631c73a4991e0001"
    );
    const [txCbor, setTxCbor] = useState<string>("");
    const [externalPKH, setExternalPKH] = useState<string | null>(
        "e48dea1ea0c60cfeed371d456419c10e83c72d772ec4631c73a4991e"
    );
    const [signElsewhere, setSignElsewhere] = useState<boolean>(false);

    const Redeemer = Data.Object({
        proof: MerkleProof,
        pkh: PubKeyHash,
        n: Data.Integer(),
    });
    type Redeemer = Data.Static<typeof Redeemer>;

    async function prepareMintThreadTokenTx() {
        if (wAddr && lucid && cbor) {
            // Participant's public key hash
            const pkh: string = signElsewhere
                ? externalPKH || ""
                : getAddressDetails(wAddr).paymentCredential?.hash || "";

            // Thread token
            const threadScript: MintingPolicy =
                setupData.threadScript as MintingPolicy;
            const threadPol: PolicyId =
                lucid.utils.mintingPolicyToId(threadScript);
            const threadTkn: Unit = toUnit(threadPol, pkh);

            // State
            const stateValidator: SpendingValidator =
                setupData.stateValidator as SpendingValidator;
            const stateAddress: Address =
                lucid.utils.validatorToAddress(stateValidator);
            const stateTkn: Unit = setupData.stateToken;
            const utxoAtScript: UTxO[] = await lucid.utxosAt(stateAddress);
            const ourUTxO: UTxO[] = utxoAtScript.filter(
                (utxo) => utxo.assets[stateTkn] == 1n
            );

            // Participant's Merkle proof
            const merkleProof = await genStartMerkleProof(participantId);

            // Participant's validator
            const lockingValidatorParticipant: SpendingValidator = {
                type: "PlutusV2",
                script: cbor,
            };
            const lockingAddressParticipant: Address =
                lucid.utils.validatorToAddress(lockingValidatorParticipant);

            console.log(
                "lockingValidatorParticipant: ",
                lockingValidatorParticipant
            );
            console.log(
                "lockingAddressParticipant: ",
                lockingAddressParticipant
            );

            if (ourUTxO && ourUTxO.length > 0 && ourUTxO[0].datumHash) {
                const stateDatum = await lucid.provider.getDatum(
                    ourUTxO[0].datumHash
                );
                console.log("old state: " + Data.from(stateDatum));
                const newState = setBit(Data.from(stateDatum), participantId);
                console.log("new state: " + newState);

                const redeemer: Redeemer = {
                    proof: merkleProof,
                    pkh: pkh,
                    n: BigInt(participantId),
                };

                console.log("StartExam -> redeemer: ", redeemer);

                const tx = await lucid
                    .newTx()
                    .collectFrom(ourUTxO, Data.to(new Constr(0, [])))
                    .attachSpendingValidator(stateValidator)
                    .payToContract(stateAddress, Data.to(newState), {
                        [stateTkn]: 1n,
                    })
                    .mintAssets(
                        { [threadTkn]: 1n },
                        Data.to<Redeemer>(redeemer, Redeemer)
                    )
                    .attachMintingPolicy(threadScript)
                    .payToContract(lockingAddressParticipant, Data.to(pkh), {
                        [threadTkn]: 1n,
                    })
                    .addSignerKey(pkh)
                    .complete({ nativeUplc: false });

                return tx;
            } else {
                alert("No UTxO's found that can be burned!");
            }
        }
    }

    async function mintThreadToken() {
        if (wAddr && lucid && cbor) {
            const tx = await prepareMintThreadTokenTx();
            if (!tx) return;
            await signAndSubmitTx(tx);
        }
    }

    async function printThreadTokenTx() {
        if (wAddr && lucid && cbor) {
            const tx = await prepareMintThreadTokenTx();
            if (!tx) return;
            setTxCbor(tx.toString());
        }
    }

    return (
        <div className="flex flex-col ">
            <TextContainer>
                <p className="text-lg mb-2">
                    {" "}
                    <b>
                        Welcome to the{" "}
                        <span className=" text-blue-600">
                            Plutus Pioneer Program
                        </span>{" "}
                        certification exam!{" "}
                    </b>
                </p>
                <p>To complete the exam, you&apos;ll need to:</p>
                <ul className=" list-disc">
                    <li className="list-inside">
                        Get your BlockFrost key and paste it into the input on
                        the right.
                    </li>
                    <li className="list-inside">
                        Have Nami wallet in your browser.
                    </li>
                    <li className="list-inside">
                        Use the same public key hash and Name you provided to
                        IOG and the ID IOG gave you.
                    </li>
                </ul>
            </TextContainer>
            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p className="px-2">
                    I want to sign the transaction elsewhere:
                </p>
                <input
                    className="h-6 w-6"
                    type="checkbox"
                    checked={signElsewhere}
                    onChange={(e) => setSignElsewhere(e.target.checked)}
                ></input>
            </div>
            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p>Participant&apos;s ID as shared by IOG:</p>
                <input
                    type="number"
                    value={participantId}
                    onChange={(e) => setParticipantId(Number(e.target.value))}
                    className=" w-40 py-1 px-2 ml-2 border border-zinc-700 rounded"
                />
            </div>

            <div className="flex flex-row w-full justify-center items-center my-8 text-lg text-zinc-800 font-quicksand ">
                <p>Participant&apos;s CBOR as shared by IOG:</p>
                <input
                    type="text"
                    value={cbor ?? ""}
                    onChange={(e) => setCbor(e.target.value)}
                    className=" w-40 py-1 px-2 ml-2 border border-zinc-700 rounded"
                />
            </div>
            {!signElsewhere ? (
                <button
                    onClick={mintThreadToken}
                    disabled={!wAddr || !lucid || !cbor}
                    className=" bg-zinc-800 text-white font-quicksand text-lg font-bold py-3 px-8 mr-5 rounded-lg shadow-[0_5px_0px_0px_rgba(0,0,0,0.6)] active:translate-y-[2px] active:shadow-[0_4px_0px_0px_rgba(0,0,0,0.6)] disabled:active:translate-y-0 disabled:active:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:bg-zinc-200 disabled:shadow-[0_5px_0px_0px_rgba(0,0,0,0.2)] disabled:text-zinc-600"
                >
                    {" "}
                    Start exam (Mint Thread Token)
                </button>
            ) : (
                <SignElswhere
                    externalPKH={externalPKH || ""}
                    setExternalPKH={setExternalPKH}
                    print={printThreadTokenTx}
                    txCbor={txCbor}
                    disabled={!wAddr || !lucid || !externalPKH || !cbor}
                />
            )}
        </div>
    );
}
