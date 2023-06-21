{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}


module StateNFT where

import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext, mkValidatorScript,
                                            Validator, ScriptContext (..), TokenName, TxOutRef (..), TxInfo (..), unsafeFromBuiltinData, TxId (..), TxInInfo (..))
import           PlutusTx                   (compile, CompiledCode)
import           PlutusTx.Prelude           (Bool (..), ($), traceIfFalse, any, (==), (&&))
import           Utilities                  (wrapPolicy, writeValidatorToFile, wrapValidator, writeCodeToFile)
import           Prelude                    (IO)
import           Plutus.V1.Ledger.Value     (flattenValue)

-- A simple NFT policy for the state token

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                             traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == 1
        _                -> False

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedNFTPolicy tid ix tn' = wrapPolicy $ mkNFTPolicy oref tn
  where
    oref :: TxOutRef
    oref = TxOutRef
        (TxId $ unsafeFromBuiltinData tid)
        (unsafeFromBuiltinData ix)

    tn :: TokenName
    tn = unsafeFromBuiltinData tn'

nftCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(PlutusTx.compile [|| mkWrappedNFTPolicy ||])

saveNFTCode :: IO ()
saveNFTCode = writeCodeToFile "assets/state-nft-policy.plutus" nftCode