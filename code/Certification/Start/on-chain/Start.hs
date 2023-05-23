{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}


module Start where

import           Plutus.V2.Ledger.Api (BuiltinData, ScriptContext,CurrencySymbol, mkValidatorScript,
                                       Validator, ScriptContext (..), TxOutRef, ScriptPurpose (..))
import           PlutusTx             (compile, makeIsDataIndexed)
import           PlutusTx.Prelude     (Bool (..),BuiltinByteString,(&&),Integer, error)
import           Utilities            (writeValidatorToFile, wrapValidator)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT

-- [General notes on this file]
-- This file contains two plutus scripts, the minting logic of the thread token and the logic of the validator that locks the 
-- state that keeps track of the participant who started the exam / can still start the exam.

--------------------- The state validator -------------

-- | 'Parameters' data type holds all the necessary information needed for setting up the minting policy.
data Parameters = Parameters {
                    -- | the Merkle root of the participant data. A member is the concatenation of a public key hash and a integer.
                    merkleRoot          :: MT.Hash
    ,
                    -- | the currency symbol of the tokens that are locked with the state.
                    statePolicy         :: CurrencySymbol
}
-- Ensure Plutus data indexing is fixed properly for the 'Parameters' type.
makeIsDataIndexed ''Parameters [('Parameters,0)]

data Redeemer = Unlock | SetBit Integer
-- Ensure Plutus data is indexed properly for the 'Redeemer' type.
makeIsDataIndexed ''Redeemer [('Unlock,0),('SetBit,1)]

{-# INLINABLE  mkStateScript #-}
mkStateScript :: BuiltinByteString -> Redeemer -> ScriptContext -> Bool
mkStateScript _bs red ctx = case red of
    Unlock      -> True -- implement check if all ones later
    SetBit n    -> checkNotTheSame && checkUpdatedState n
    where
        checkNotTheSame :: Bool
        checkNotTheSame = True

        checkUpdatedState :: Integer -> Bool 
        checkUpdatedState _n = True

        -- 'txOutRef' represents the transaction reference (Id + Index) of the output associated with the currently script being checked.
        _txOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose=Spending ref} = ref
                getRef _ = error ()

{-# INLINABLE  mkWrappedStateScript #-}
mkWrappedStateScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedStateScript = wrapValidator mkStateScript

stateValidator :: Validator
stateValidator = mkValidatorScript $$(compile [|| mkWrappedStateScript ||])

saveStateValidator :: IO ()
saveStateValidator = writeValidatorToFile "assets/stateValidator.plutus" stateValidator
