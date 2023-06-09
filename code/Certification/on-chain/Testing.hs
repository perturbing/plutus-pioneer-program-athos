{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}


module Testing where

import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext,
                                            MintingPolicy, mkMintingPolicyScript, mkValidatorScript,
                                            Validator, ScriptContext (..))
import           PlutusTx                   (compile)
import           PlutusTx.Prelude           (Bool (..))
import           Utilities                  (wrapPolicy, writePolicyToFile,
                                            writeValidatorToFile, wrapValidator)
import           Prelude                    (IO)

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