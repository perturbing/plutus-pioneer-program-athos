{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Minting where

import           Plutus.V2.Ledger.Api (BuiltinData, PubKeyHash, ScriptContext,CurrencySymbol,
                                       MintingPolicy, mkMintingPolicyScript, mkValidatorScript,
                                       Validator, Address, ScriptContext (..), TxInfo (..),
                                       TxInInfo (..), TokenName (..), Value (..),TxOut,txOutValue,
                                       txOutAddress,txOutDatum,OutputDatum (..), DatumHash (..), TxOut (..),
                                       Datum (..),PubKeyHash (..))
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol, findDatum,txSignedBy)
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.AssocMap
import           PlutusTx.Maybe
import           PlutusTx.Prelude     (Bool (..),BuiltinByteString,(==),($),(&&),Integer, error,
                                      otherwise, (<>),(<$>),find)
import           Utilities            (wrapPolicy, writeCodeToFile,writePolicyToFile,currencySymbol,
                                      writeValidatorToFile, wrapValidator)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-VALIDATOR ----------------------------------
--  This minting policy allows for minting of a unique NFT (Non-Fungible Token) upon burning of the thread token,
--  along with a merkle proof of the datum. The 'mkNFTPolicy' function below holds the core logic of this policy.


-- | 'Parameters' data type holds all the necessary information needed for setting up the minting policy.
data Parameters = Parameters {
      merkleRoot        :: MT.Hash             -- the Merkle root of the NFT metadata. a member is the concatenation of public key hash the datum hash.
    , prefixNFT         :: BuiltinByteString   -- the prefix (222) for NFT. This is a 4-byte identifier that denotes the token is meant to be a NFT.
    , prefixRef         :: BuiltinByteString   -- the prefix (100) for references. This is a 4-byte identifier that denotes the token is meant to be a reference token.
    , threadSymbol      :: CurrencySymbol      -- the currency symbol of the thread token, note that the token name holds the public key hash of the participant.
    , lockAddress       :: Address             -- the predetermined address to lock the reference NFT at.
}

-- Ensure Plutus data indexing is fixed properly for the 'Parameters' type.
makeIsDataIndexed ''Parameters [('Parameters,0)]

-- | Under this minting policy, a participant can either mint or burn the NFT.
-- | Minting requires a Merkle membership proof.
data Redeemer = Mint MT.Proof | Burn
-- Ensure Plutus data is indexed properly for the 'Redeemer' type.
makeIsDataIndexed ''Redeemer [('Mint,0),('Burn,1)]

-- | The core minting policy of the Certificate.
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: Parameters -> Redeemer -> ScriptContext -> Bool
mkNFTPolicy params red ctx = case red of
    Mint proof  -> checkValues && checkDatumAddr && checkMember proof && txSignedBy txInfo (PubKeyHash pkh) -- Minting is possible if all these conditions are met.
    Burn        -> True -- Currently burning is always possible, this will change to check that both the (222) and (100) token are burned together.
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

{-# INLINABLE splitValue #-}
-- `splitValue` is a utility function that takes a currency symbol and a value, 
-- and returns a tuple of total value under the provided currency symbol and the residual value.
splitValue :: CurrencySymbol -> Value -> (Map TokenName Integer, Value)
splitValue symbol val
    | member symbol val'  = (maybe empty (\x -> x) (lookup symbol val'), Value (delete symbol val'))
    | otherwise           = error ()
    where val' = getValue val

{-# INLINABLE  mkNFTWrapped #-}
mkNFTWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkNFTWrapped params = wrapPolicy $ mkNFTPolicy params'
    where
        params' = unsafeFromBuiltinData params

nftCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> () )
nftCode = $$(compile [|| mkNFTWrapped ||])

saveNFTPolicy :: IO ()
saveNFTPolicy = writeCodeToFile "assets/certificate-policy.plutus" nftCode

---------------------- The never fail minting policy ------------

{-# INLINABLE  mkFree #-}
mkFree :: () -> ScriptContext -> Bool
mkFree _red _ctx = True

{-# INLINABLE  mkWrappedFree #-}
mkWrappedFree :: BuiltinData -> BuiltinData -> ()
mkWrappedFree = wrapPolicy $ mkFree

freePolicy :: MintingPolicy
freePolicy = mkMintingPolicyScript $$(compile [|| mkWrappedFree ||])

saveFreePolicy :: IO ()
saveFreePolicy = writePolicyToFile "assets/alwaysTrue.plutus" freePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = currencySymbol freePolicy

--------------------- The always fail validator -------------
-- | TO DO: create a locking script that allows to spend, if above m at script is burned
{-# INLINABLE  mkAlwaysFail #-}
mkAlwaysFail :: () -> () -> ScriptContext -> Bool
mkAlwaysFail _dat _red _ctx = False

{-# INLINABLE  mkWrappedAlwaysFail #-}
mkWrappedAlwaysFail :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedAlwaysFail = wrapValidator $ mkAlwaysFail

alwaysFail :: Validator
alwaysFail = mkValidatorScript $$(compile [|| mkWrappedAlwaysFail ||])

saveAlwaysFail :: IO ()
saveAlwaysFail = writeValidatorToFile "assets/alwaysFalse.plutus" alwaysFail