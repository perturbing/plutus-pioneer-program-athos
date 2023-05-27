{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Start where

import           Plutus.V2.Ledger.Api           (BuiltinData (..), ScriptContext,CurrencySymbol, ScriptContext (..), TxOutRef, ScriptPurpose (..), TxInfo (..)
                                                , Value (..), TokenName (..), UnsafeFromData (..), PubKeyHash (..), TxOut (..), DatumHash (..), Address (..),
                                                Credential (..), ValidatorHash (..), TxInInfo (..), Redeemer (..), FromData (..), Datum (..))
import           Plutus.V2.Ledger.Tx            (OutputDatum(..))
import           Plutus.V2.Ledger.Contexts      (ownCurrencySymbol, txSignedBy, findDatum)
import           PlutusTx                       (compile, makeIsDataIndexed, CompiledCode)
import           PlutusTx.Prelude               (Bool (..),BuiltinByteString,(&&),Integer, error, maybe, otherwise, ($), foldr, (<>), (==), find, isJust,
                                                Maybe (..), any, lengthOfByteString, (*), (-), (/=), mempty)
import           PlutusTx.AssocMap              (Map, empty, member, lookup, delete, singleton, keys)
import qualified Plutus.MerkleTree              as MT
import           Plutus.Crypto.Number.Serialize (i2osp)
import           Plutus.Data.Bits               (setBit)
import           Utilities                      (wrapValidator, writeCodeToFile, wrapPolicy)
import           Prelude                        (IO)

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

----------------------------------- MINTING-VALIDATOR ---------------------------------------------
-- TODO: General note on what this minting policy does.

-- | 'Parameters' data type holds all the necessary information needed for setting up the minting policy.
data Parameters = Parameters {
                    -- | the Merkle root of the participant data. A member is the concatenation of a public key hash and a integer.
                    merkleRoot          :: MT.Hash
    ,
                    -- | the currency symbol of the tokens that are locked with the state.
                    stateSymbol         :: CurrencySymbol
}
-- Ensure Plutus data indexing is fixed properly for the 'Parameters' type.
makeIsDataIndexed ''Parameters [('Parameters,0)]

-- | Under this minting policy, a participant can mint their thread token.
-- | Minting requires a Merkle membership proof, their public key hash, and their participant number.
-- | Burning is always allowed.
data MintRedeemer = Mint MT.Proof BuiltinByteString Integer | Burn
--data MintRedeemer = Mint Integer | Burn
-- Ensure Plutus data is indexed properly for the 'MintRedeemer' type.
makeIsDataIndexed ''MintRedeemer [('Mint,0),('Burn,1)]

-- | The core minting policy of the Certificate.
{-# INLINABLE mkThreadPolicy #-}
mkThreadPolicy :: Parameters -> MintRedeemer -> ScriptContext -> Bool
mkThreadPolicy params red ctx = case red of
    -- Minting is possible if all these conditions are met.
    Mint proof pkh n        -> foldr (&&) (txSignedBy txInfo (PubKeyHash pkh)) [checkMember proof pkh n, checkMintValue pkh, checkStateNFT]
    -- Burning is always possible, as long as a transaction burns 1 thread token at a time.
    Burn                    -> singleton (TokenName pkh') (-1) == ownPolMint
    where
        -- Checks that the concatination of the public key hash, participant number, scripthash and datum hash are a member of the merkle tree
        checkMember :: MT.Proof -> BuiltinByteString -> Integer -> Bool
        checkMember proof pkh n = MT.member (pkh <> i2osp n <> threadScriptHash <> threadDatumHash) (merkleRoot params) proof

        -- Check that only the thread token is minted with the correct name (the pkh that is in the merkle tree)
        checkMintValue :: BuiltinByteString -> Bool
        checkMintValue pkh = singleton (TokenName pkh) 1 == ownPolMint && res == mempty

        -- Check that this transaction contains the state NFT that keeps track of who started their exam.
        checkStateNFT :: Bool 
        checkStateNFT = any filter (txInfoInputs txInfo)
            where
                filter :: TxInInfo -> Bool
                filter TxInInfo{txInInfoResolved=TxOut{txOutValue}} = member (stateSymbol params) $ getValue txOutValue

        -- Rest of the code are variable initializations and auxiliary function definitions to support the checks above.

        -- `txInfo` is the transaction information of the current transaction context.
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- `ownCur` is the currency symbol of this minting script.
        ownCur :: CurrencySymbol
        ownCur = ownCurrencySymbol ctx

        -- `splitValue` is called on the mint value in the transaction context to get values of this minting script's policy.
        (ownPolMint,res) = splitValue ownCur (txInfoMint txInfo)

        -- `pkh'` is the public key hash of the participant, which is extracted from the token name of the thread token.
        pkh' :: BuiltinByteString
        pkh' = (\(x:_xs)-> unTokenName x) (keys ownPolMint)

        -- `threadUtxo` is the transaction output that contains the thread token.
        threadUtxo :: TxOut
        threadUtxo = maybe (error ()) (\x -> x) (find myfilter (txInfoOutputs txInfo))
            where
                myfilter :: TxOut -> Bool
                myfilter output = member ownCur (getValue (txOutValue output)) 

        -- `refDatumHash` is the datum hash of the reference transaction output.
        -- Even though it's not an inline datum, the full datum is validated to be attached in the witness set.
        threadDatumHash :: BuiltinByteString
        threadDatumHash = fetchDatum threadUtxo
            where
                fetchDatum :: TxOut -> BuiltinByteString
                fetchDatum TxOut{txOutDatum} = case txOutDatum of
                    NoOutputDatum                   -> error ()
                    OutputDatumHash (DatumHash h)   -> if isJust (findDatum (DatumHash h) txInfo) then h else error ()
                    OutputDatum _                   -> error ()

        -- 'threadScriptHash` is the script hash of the datum of the utxo that locks the thread token.
        threadScriptHash :: BuiltinByteString
        threadScriptHash = fetchScriptHash threadUtxo
            where
                fetchScriptHash :: TxOut -> BuiltinByteString
                fetchScriptHash TxOut{txOutAddress=Address (ScriptCredential (ValidatorHash h)) Nothing} = h
                fetchScriptHash _ = error ()

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

data MyRedeemer = SetBit | Unlock 
-- Ensure Plutus data is indexed properly for the 'MyRedeemer' type.
makeIsDataIndexed ''MyRedeemer [('SetBit,0),('Unlock,1)]

{-# INLINABLE  mkStateScript #-}
-- This script is parametrized over the currency symbol of the above minting policy.
mkStateScript :: CurrencySymbol -> BuiltinByteString -> MyRedeemer -> ScriptContext -> Bool
mkStateScript threadSymbol dtm red ctx = case red of
    -- A participant that is a member of the merkle tree can update the state if they mint their thread token in the same transaction.
    SetBit  -> checkStateUpdate bitNumber && checkStateAddress
    -- In case all participants started their exam, the state utxo can be unlocked.
    -- For two participants that is if datum = Ob11 (in BE notation that is a bytestring with one byte (00000011))
    -- given that these two particpants get assigned the last two bits as their index in the merkle tree
    Unlock  -> dtm == i2osp 0b11
    where
        -- Checks that the state is updated correctly, that is one bit is set from 0 to 1, and its index is part of the merkle tree. 
        checkStateUpdate :: Integer -> Bool
        checkStateUpdate n = newDatum == setBit dtm (lengthOfByteString dtm * 8 - 1 - n) && newDatum /= dtm

        -- Checks that the utxo that holds the state, never leaves its current address.
        checkStateAddress :: Bool 
        checkStateAddress = txOutAddress refUtxo == txOutAddress newRefUtxo

        -- Rest of the code are variable initializations and auxiliary function definitions to support the checks above.

        -- `txInfo` is the transaction information of the current transaction context.
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- 'mintRedeemer' is the redeemer of the thread token minting policy that is defined above.
        -- OPEN QUESTION: Can anyone inject a script purpose + redeemer in this map? 
        -- Or is it strictly constructed by the nodes that validate the network?
        -- The datum hash / datum map in the context is injectable.
        mintRedeemer :: Redeemer
        mintRedeemer = maybe (error ()) (\x -> x) (lookup (Minting threadSymbol) (txInfoRedeemers txInfo))

        -- 'bitNumber' is the bit number that needs to be flipt according to the merkle proof in the above minting script.
        bitNumber :: Integer
        bitNumber = case fromBuiltinData (getRedeemer mintRedeemer) of
          Just (Mint _ _ n) -> n
          Just Burn         -> error ()
          Nothing           -> error ()

        -- 'txOutRef' represents the transaction reference (Id + Index) of the output associated with the currently script being checked.
        txOutRef :: TxOutRef
        txOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose= Spending ref} = ref
                getRef _ = error ()

        -- `refUtxo` fetches the transaction output that corresponds to `txOutRef`.
        refUtxo :: TxOut
        refUtxo = maybe (error ()) txInInfoResolved $ find (\TxInInfo{txInInfoOutRef} -> txOutRef == txInInfoOutRef) (txInfoInputs txInfo)

        -- `ownValue` is the value associated with `txOutRef`, excluding Ada.
        ownValue :: Value
        ownValue = txOutValue refUtxo

        -- 'newRefUtxo' is the newly create output that contains the state NFT. 
        -- We look for an output with the same value, above we check that this output has the same Address as the old 'refUtxo'.
        newRefUtxo :: TxOut
        newRefUtxo = maybe (error ()) (\x->x) $ find (\TxOut{txOutValue}-> txOutValue == ownValue) (txInfoOutputs txInfo)

        -- 'newRefDatumHash' is the datum hash of the newly create output that contains the state NFT.
        -- Note that this script expects the datum as a hash, not an inline datum.
        newRefDatumHash :: DatumHash
        newRefDatumHash = fetchDatum newRefUtxo
            where
                fetchDatum :: TxOut -> DatumHash
                fetchDatum TxOut{txOutDatum} = case txOutDatum of
                    NoOutputDatum                   -> error ()
                    OutputDatumHash d               -> d
                    OutputDatum _                   -> error ()

        -- 'newDatum' is the new datum (a BuiltinByteString) that represents the updated state.
        newDatum :: BuiltinByteString
        newDatum = maybe (error ()) convertDatum $ findDatum newRefDatumHash txInfo
            where
                convertDatum :: Datum -> BuiltinByteString
                convertDatum datum = case fromBuiltinData (getDatum datum) of
                    Just bs     -> bs
                    Nothing     -> error ()

{-# INLINABLE  mkWrappedStateScript #-}
mkWrappedStateScript :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedStateScript symbol = wrapValidator $ mkStateScript symbol'
    where 
        symbol' = unsafeFromBuiltinData symbol

stateValCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> () )
stateValCode = $$(compile [|| mkWrappedStateScript ||])

saveStateValidator:: IO ()
saveStateValidator = writeCodeToFile "assets/start-state-validator.plutus" stateValCode

