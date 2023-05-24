{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Start where

import           Plutus.V2.Ledger.Api (BuiltinData, ScriptContext,CurrencySymbol, mkValidatorScript,
                                       Validator, ScriptContext (..), TxOutRef, ScriptPurpose (..), TxInfo (..), Value (..), TokenName, UnsafeFromData (..), PubKeyHash (..))
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode)
import           PlutusTx.Prelude     (Bool (..),BuiltinByteString,(&&),Integer, error, maybe, otherwise, ($), foldr, (<>), (<), (==), consByteString, emptyByteString, quotient, remainder)
import           Utilities            (writeValidatorToFile, wrapValidator, writeCodeToFile, wrapPolicy)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT
import PlutusTx.AssocMap (Map, empty, member, lookup, delete)
import Plutus.V2.Ledger.Contexts (ownCurrencySymbol, txSignedBy)

-- [General notes on this file]
-- This file contains two plutus scripts, the minting logic of the thread token and the logic of the validator that locks the 
-- state that keeps track of the participant who started the exam / can still start the exam.

--------------------- helper functions -------------

{-# INLINABLE splitValue #-}
-- | `splitValue` is a utility function that takes a currency symbol and a value, 
-- | and returns a tuple of total value under the provided currency symbol and the residual value.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = (empty,val)
    where val' = getValue val

{-# INLINABLE i2osp #-}
-- | i2osp converts a positive integer into a builtin byte string
--   Plutus version of `(Crypto.Number.Serialize.i2osp)`
--   As per rfc3447, the first byte is the most significant byte.
--   This function will give an error for a negative integer.
i2osp :: Integer -> BuiltinByteString
i2osp n
    | n < 0     = error ()
    | n == 0    = consByteString 0 emptyByteString
    | otherwise = go n
    where go m 
            | m == 0    = emptyByteString
            | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString

----------------------------------- MINTING-VALIDATOR ---------------------------------------------
-- TODO: General note on what this minting policy does.

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

-- | Under this minting policy, a participant can mint their thread token.
-- | Minting requires a Merkle membership proof, their public key hash, and their participant number.
-- | Burning is always allowed.
data MintRedeemer = Mint MT.Proof BuiltinByteString Integer | Burn
-- Ensure Plutus data is indexed properly for the 'MintRedeemer' type.
makeIsDataIndexed ''MintRedeemer [('Mint,0),('Burn,1)]

-- | The core minting policy of the Certificate.
{-# INLINABLE mkThreadPolicy #-}
mkThreadPolicy :: Parameters -> MintRedeemer -> ScriptContext -> Bool
mkThreadPolicy params red ctx = case red of
    Mint proof pkh n        -> foldr (&&) True [checkMember proof pkh n, txSignedBy txInfo (PubKeyHash pkh), True]
    Burn                    -> True -- TODO: check that only tokens burned.
    where
        -- Checks that the concatination of the public key hash, participant number, scripthash and datum hash are a member of the merkle tree
        checkMember :: MT.Proof -> BuiltinByteString -> Integer -> Bool
        checkMember proof pkh n = MT.member (pkh <> i2osp n) (merkleRoot params) proof
        -- Rest of the code are variable initializations and auxiliary function definitions to support the checks above.

        -- `txInfo` is the transaction information of the current transaction context.
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- `ownCur` is the currency symbol of this minting script.
        ownCur :: CurrencySymbol
        ownCur = ownCurrencySymbol ctx

        -- `splitValue` is called on the mint value in the transaction context to get values of this minting script's policy.
        (_ownPolMint,_res) = splitValue ownCur (txInfoMint txInfo)

{-# INLINABLE  mkThreadWrapped #-}
mkThreadWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkThreadWrapped params = wrapPolicy $ mkThreadPolicy params'
    where
        params' = unsafeFromBuiltinData params

threadCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> () )
threadCode = $$(compile [|| mkThreadWrapped ||])

saveThreadPolicy :: IO ()
saveThreadPolicy = writeCodeToFile "assets/start-policy.plutus" threadCode

--------------------- The state validator -------------

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

