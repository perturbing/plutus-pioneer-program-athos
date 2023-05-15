{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Minting where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator,
                                       mkValidatorScript)
import           PlutusTx             (compile, unstableMakeIsData, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude     (lengthOfByteString,Bool (..),BuiltinByteString,(==),($))
import           Utilities            (wrapPolicy, writeCodeToFile)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-VALIDATOR ----------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: MT.Hash -> (BuiltinByteString, MT.Proof) -> ScriptContext -> Bool
mkPolicy root (mem, proof) _ctx = MT.member mem root proof

{-# INLINABLE  mkWrappedPolicy #-}
mkWrappedPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicy root = wrapPolicy $ mkPolicy root'
    where
        root' = unsafeFromBuiltinData root

policyCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> () )
policyCode = $$(compile [|| mkWrappedPolicy ||])

saveVal :: IO ()
saveVal = writeCodeToFile "Mint/assets/policy.plutus" policyCode