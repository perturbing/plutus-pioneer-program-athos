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

module Mint where

import           Plutus.V2.Ledger.Api (BuiltinData, ScriptContext,CurrencySymbol,
                                       MintingPolicy, mkMintingPolicyScript, mkValidatorScript,
                                       Validator, ScriptContext (..), TxInfo (..),
                                       TxInInfo (..), TokenName (..), Value (..),TxOut ,txOutValue,
                                       txOutAddress, txOutDatum, OutputDatum (..), DatumHash (..), TxOut (..),
                                       adaSymbol, PubKeyHash (..), ScriptPurpose (..), Address, TxOutRef)
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol, findDatum, txSignedBy)
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import Plutus.V1.Ledger.Value         (flattenValue)
import PlutusTx.AssocMap              (delete, empty, insert, keys, lookup, member, singleton, Map )
import PlutusTx.Maybe                 (isJust, maybe )
import PlutusTx.Eq                    (Eq(..) )
import           PlutusTx.Prelude     (Bool (..), BuiltinByteString, ($), (&&), Integer, error,
                                      otherwise, (<>), (<$>), find, foldr,
                                      map, elem, negate)
import           Utilities            (wrapPolicy, writeCodeToFile,writePolicyToFile, currencySymbol,
                                      writeValidatorToFile, wrapValidator)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT

-- [General notes on this file]
-- This file contains two plutus scripts, the minting logic of the PPP NFT and the logic of the validator that locks the 
-- reference token of this NFT (See CIP 68 for more info on this reference token).

---------------------------------------------------------------------------------------------------
----------------------------------- MINTING-VALIDATOR ---------------------------------------------
--  This minting policy allows for minting of a unique NFT (Non-Fungible Token) upon burning of the thread token,
--  along with a merkle proof of the datum. The 'mkNFTPolicy' function below holds the core logic of this policy.


-- | 'Parameters' data type holds all the necessary information needed for setting up the minting policy.
data Parameters = Parameters {
                    -- | the Merkle root of the NFT metadata. a member is the concatenation of public key hash the datum hash.
                    merkleRoot        :: MT.Hash
    ,
                    -- | the prefix (222) for NFT. This is a 4-byte identifier that denotes the token is meant to be a NFT.
                    prefixNFT         :: BuiltinByteString
    ,
                    -- | the prefix (100) for references. This is a 4-byte identifier that denotes the token is meant to be a reference token.
                    prefixRef         :: BuiltinByteString
    ,
                    -- | the currency symbol of the thread token, note that the token name holds the public key hash of the participant.
                    threadSymbol      :: CurrencySymbol
    ,
                    -- | the predetermined address to lock the reference NFT at.
                    lockAddress       :: Address
}

-- Ensure Plutus data indexing is fixed properly for the 'Parameters' type.
makeIsDataIndexed ''Parameters [('Parameters,0)]

-- | Under this minting policy, a participant can either mint or burn the NFT.
-- | Minting requires a Merkle membership proof.
-- | Burning requires the public key hash associated with the token.
data Redeemer = Mint MT.Proof | Burn BuiltinByteString
-- Ensure Plutus data is indexed properly for the 'Redeemer' type.
makeIsDataIndexed ''Redeemer [('Mint,0),('Burn,1)]

-- | The core minting policy of the Certificate.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: Parameters -> Redeemer -> ScriptContext -> Bool
mkNFTPolicy params red ctx = case red of
    Mint proof  -> foldr (&&) checkValues [checkDatumAddr, checkMember proof,checkRefValue, txSignedBy txInfo (PubKeyHash pkh)] -- Minting is possible if all these conditions are met.
    Burn pkh'   -> ownPolMint == insert (TokenName (prefixNFT params <> pkh')) (-1) (singleton (TokenName (prefixRef params <> pkh')) (-1)) -- burning can only happen is you burn both reference and NFT token
    where
        -- Checks that the thread token is burned and one reference and one user NFT are minted.
        checkValues :: Bool
        checkValues = ownPolMint == insert (TokenName (prefixNFT params <> pkh)) 1 (singleton (TokenName (prefixRef params <> pkh)) 1) &&
                      maybe False (\x -> x== -1) (lookup (TokenName pkh) threadPolMint)

        -- Checks that the reference token is sent to the predetermined locking address.
        checkDatumAddr :: Bool
        checkDatumAddr = lockAddress params == txOutAddress refUtxo

        -- Checks that the datum and public key hash are a member of the Merkle tree, confirming valid participant.
        checkMember :: MT.Proof -> Bool
        checkMember proof = MT.member (pkh <> refDatumHash) (merkleRoot params) proof

        -- Checks that the value send to the lock adress only contains the reference token.
        -- This is needed since the lock script at this address only unlocks if all value is burned
        -- So it cannot contain any tokens of which their policy logic is unknown.
        checkRefValue :: Bool
        checkRefValue = singleton ownCur (singleton (TokenName (prefixRef params <> pkh)) 1) == delete adaSymbol (getValue (txOutValue refUtxo))

        -- Rest of the code are variable initializations and auxiliary function definitions to support the checks above.

        -- `txInfo` is the transaction information of the current transaction context.
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- `ownCur` is the currency symbol of this minting script.
        ownCur :: CurrencySymbol
        ownCur = ownCurrencySymbol ctx

        -- `splitValue` is called on the mint value in the transaction context to get values of the thread token policy and this minting script's policy.
        (threadPolMint,res1) = splitValue (threadSymbol params) (txInfoMint txInfo)
        (ownPolMint,_) = splitValue ownCur res1

        -- `pkh` is the public key hash of the participant, which is extracted from the token name of the thread token.
        -- This step also validates that only one token is burned under this thread policy.
        pkh :: BuiltinByteString
        pkh = (\(x:xs)-> if xs == [] then unTokenName x else error ()) (keys threadPolMint)

        -- `refUtxo` is the transaction output that contains the reference NFT.
        refUtxo :: TxOut
        refUtxo = maybe (error ()) (\x -> x) (find myfilter (txInfoOutputs txInfo))
            where
                myfilter :: TxOut -> Bool
                myfilter output = maybe False (\x -> x) $ member (TokenName (prefixRef params <> pkh)) <$> lookup ownCur (getValue (txOutValue output))

        -- `refDatumHash` is the datum hash of the reference transaction output.
        -- Even though it's not an inline datum, the full datum is validated to be attached in the witness set.
        refDatumHash :: BuiltinByteString
        refDatumHash = fetchDatum refUtxo
            where
                fetchDatum :: TxOut -> BuiltinByteString
                fetchDatum TxOut{txOutDatum} = case txOutDatum of
                    NoOutputDatum                   -> error ()
                    OutputDatumHash (DatumHash h)   -> if isJust (findDatum (DatumHash h) txInfo) then h else error ()
                    OutputDatum _                   -> error ()


{-# INLINABLE  mkNFTWrapped #-}
mkNFTWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkNFTWrapped params = wrapPolicy $ mkNFTPolicy params'
    where
        params' = unsafeFromBuiltinData params

nftCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> () )
nftCode = $$(compile [|| mkNFTWrapped ||])

saveNFTPolicy :: IO ()
saveNFTPolicy = writeCodeToFile "assets/certificate-policy.plutus" nftCode

---------------------- The never fail minting policy ----------------------
-- Since the thread minting policy is not written yet, and we do want to test out the above script
-- the always true minting policy is currently used as a standin for the soon to follow real thread policy.
-- TO DO: Remove this once the "Start" exam functionality is done.

{-# INLINABLE  mkFree #-}
mkFree :: () -> ScriptContext -> Bool
mkFree _red _ctx = True

{-# INLINABLE  mkWrappedFree #-}
mkWrappedFree :: BuiltinData -> BuiltinData -> ()
mkWrappedFree = wrapPolicy mkFree

freePolicy :: MintingPolicy
freePolicy = mkMintingPolicyScript $$(compile [|| mkWrappedFree ||])

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "assets/alwaysTrue-policy.plutus" freePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = currencySymbol freePolicy


--------------------- The locing validator -------------
-- This section details the logic for the locking validator. This mechanism holds the reference token of the PPP NFT as described in CIP 68.
-- The purpose of this validator is twofold: to mint the PPP NFT and to provide an option for participants to burn them.
-- A key condition for burning is that both the reference token and the NFT should be burned together, as per the minting policy described above.
-- However, before the burning of the reference token can occur, it must first be released from the locking validator.
-- As the minting policy references the locking address in its parameters, we cannot directly refer to the minting policy from within the locking validator.
-- Hence, this validator is designed such that it verifies it can expend its output only when all value at its output (except Ada) is set to be burned.

-- Creating an Eq instance for a triple (a,b,c) for equivalence comparison.
instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
    {-# INLINABLE (==) #-}
    (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

-- | The locking validator that locks the reference metadata of the Certificate.
{-# INLINABLE  mkLockingScript #-}
mkLockingScript :: BuiltinData -> () -> ScriptContext -> Bool
mkLockingScript _dat _red ctx = foldr (&&) True $ map (\(x,y,z) -> elem (x,y, negate z) (flattenValue mintedVal)) (flattenValue ownValue)
    where
        -- `txInfo` is the transaction information of the current transaction context.
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- `mintedVal` represents the value being minted in this transaction.
        mintedVal :: Value
        mintedVal = txInfoMint txInfo

        -- 'txOutRef' represents the transaction reference (Id + Index) of the output associated with the currently script being checked.
        txOutRef = getRef ctx
            where
                getRef :: ScriptContext -> TxOutRef
                getRef ScriptContext{scriptContextPurpose=Spending ref} = ref
                getRef _ = error ()

        -- `refUtxo` fetches the transaction output that corresponds to `txOutRef`.
        refUtxo :: TxOut
        refUtxo = maybe (error ()) txInInfoResolved (find myfilter (txInfoInputs txInfo))
            where
                -- `myfilter` is a helper function to filter for the transaction input info corresponding to `txOutRef`
                myfilter :: TxInInfo -> Bool
                myfilter TxInInfo{txInInfoOutRef} = txOutRef == txInInfoOutRef

        -- `ownValue` is the value associated with `txOutRef`, excluding Ada.
        ownValue :: Value
        ownValue = Value {getValue = delete adaSymbol (getValue (txOutValue refUtxo))}


{-# INLINABLE  mkWrappedLockingScript #-}
mkWrappedLockingScript :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedLockingScript = wrapValidator mkLockingScript

lockingValidator :: Validator
lockingValidator = mkValidatorScript $$(compile [|| mkWrappedLockingScript ||])

saveLockingValidator :: IO ()
saveLockingValidator = writeValidatorToFile "assets/lockingValidator.plutus" lockingValidator

--------------------- helper functions -------------

{-# INLINABLE splitValue #-}
-- | `splitValue` is a utility function that takes a currency symbol and a value, 
-- | and returns a tuple of total value under the provided currency symbol and the residual value.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = (empty,val)
    where val' = getValue val