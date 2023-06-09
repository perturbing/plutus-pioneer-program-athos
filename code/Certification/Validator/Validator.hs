{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Validator where

import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext,CurrencySymbol,
                                            MintingPolicy, mkMintingPolicyScript, mkValidatorScript,
                                            Validator, ScriptContext (..), TxInfo (..),
                                            TxInInfo (..), TokenName (..), Value (..),TxOut ,txOutValue,
                                            txOutAddress, txOutDatum, OutputDatum (..), DatumHash (..), TxOut (..),
                                            adaSymbol, PubKeyHash (..), ScriptPurpose (..), Address, TxOutRef, POSIXTime (..),
                                            toBuiltinData,POSIXTimeRange (..), Interval (..), LowerBound (..), UpperBound (..),
                                            Extended (..))
import           Plutus.V2.Ledger.Contexts  (ownCurrencySymbol, findDatum, txSignedBy)
import           PlutusTx                   (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData, liftCode, applyCode)
import           Plutus.V1.Ledger.Value     (flattenValue)
import           PlutusTx.AssocMap          (delete, elems ,empty, insert, keys, lookup, member, singleton, Map )
import           PlutusTx.Builtins          (blake2b_256)
import           PlutusTx.Maybe             (isJust, maybe )
import           PlutusTx.Eq                (Eq(..) )
import           PlutusTx.Prelude           (Bool (..), BuiltinByteString, ($), (&&), Integer, error,
                                            otherwise, (<>), (<$>), find, foldr, (.), toBuiltin,
                                            map, elem, negate, null, (<), (>), (||), (-), length)
import           Utilities                  (wrapPolicy, writeCodeToFile,writePolicyToFile, currencySymbol,
                                            writeValidatorToFile, wrapValidator, bytesFromHex)
import           Prelude                    (IO, show)

{-# INLINABLE validator #-}
validator :: PubKeyHash -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
validator pkh dtm rdm ctx = True -- Paste your validator logic here

myPkh = PubKeyHash . toBuiltin . bytesFromHex $ "your PubKeyHash here"
myPkh :: PubKeyHash

{-# INLINABLE  mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator pkh = wrapValidator $ validator pkh'
    where
        pkh' = unsafeFromBuiltinData pkh

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(compile [|| mkWrappedValidator ||])

saveValidator :: IO ()
saveValidator = writeValidatorToFile ("assets/"<> show myPkh <>".plutus") . mkValidatorScript $ validatorCode `applyCode` liftCode (toBuiltinData myPkh)
