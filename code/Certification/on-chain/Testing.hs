{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}


module Testing where

import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext,
                                            MintingPolicy, mkMintingPolicyScript, mkValidatorScript,
                                            Validator, ScriptContext (..), TokenName, TxOutRef (..), TxInfo (..), unsafeFromBuiltinData, TxId (..), TxInInfo (..))
import           PlutusTx                   (compile, CompiledCode)
import           PlutusTx.Prelude           (Bool (..), ($), traceIfFalse, any, (==), (&&))
import           Utilities                  (wrapPolicy, writePolicyToFile,
                                            writeValidatorToFile, wrapValidator, writeCodeToFile)
import           Prelude                    (IO)
import Plutus.V1.Ledger.Value (flattenValue)

---------------------- The never fail minting policy and validator ----------------------
-- Written for testing purposes, remove later

{-# INLINABLE  mkFreePol #-}
mkFreePol :: () -> ScriptContext -> Bool
mkFreePol _red _ctx = True

{-# INLINABLE  mkWrappedFreePol #-}
mkWrappedFreePol :: BuiltinData -> BuiltinData -> ()
mkWrappedFreePol = wrapPolicy mkFreePol

freePolicyPol :: MintingPolicy
freePolicyPol = mkMintingPolicyScript $$(compile [|| mkWrappedFreePol ||])

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "assets/alwaysTrue-policy.plutus" freePolicyPol

{-# INLINABLE  mkFreeVal #-}
mkFreeVal :: () -> () -> ScriptContext -> Bool
mkFreeVal _dat _red _ctx = True

{-# INLINABLE  mkWrappedFreeVal #-}
mkWrappedFreeVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedFreeVal = wrapValidator mkFreeVal

freeValidator :: Validator
freeValidator = mkValidatorScript $$(compile [|| mkWrappedFreeVal ||])

saveFreeValidator :: IO ()
saveFreeValidator = writeValidatorToFile "assets/alwaysTrue-validator.plutus" freeValidator

-- A NFT policy for the state token

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