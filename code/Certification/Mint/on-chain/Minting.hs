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
                                       Datum (..))
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol, findDatum)
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.AssocMap
import           PlutusTx.Maybe
import           PlutusTx.Prelude     (Bool (..),BuiltinByteString,(==),($),length,foldr,(&&),Integer, error,
                                      otherwise, (<>),traceIfFalse,(<$>),find, (.),appendByteString,
                                      decodeUtf8, BuiltinString)
import           Utilities            (wrapPolicy, writeCodeToFile,writePolicyToFile,currencySymbol,
                                      writeValidatorToFile, wrapValidator)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT
import Data.Char (GeneralCategory(CurrencySymbol))

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-VALIDATOR ----------------------------------

-- | The minting policy that upon burning of the thread token + a merkle proof of the datum, mints the PPP NFT.

data Parameters = Parameters {
      merkleRoot        :: MT.Hash             -- the merkle root of the NFT metadata (member = pubkeyhash <> datumHash) (the tkn is a section of pubkey)
    , prefixNFT         :: BuiltinByteString   -- the (222) nft prefix 
    , prefixRef         :: BuiltinByteString   -- the (100) ref prefix 
    , threadSymbol      :: CurrencySymbol      -- the currency symbol of the thread token
    , lockAddress       :: Address             -- the address to lock the reference NFT at
}
makeIsDataIndexed ''Parameters [('Parameters,0)]

data Redeemer = Mint MT.Proof | Burn
makeIsDataIndexed ''Redeemer [('Mint,0),('Burn,1)]

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: Parameters -> Redeemer -> ScriptContext -> Bool
mkNFTPolicy params red ctx = case red of
    Mint proof  -> checkValues && checkDatumAddr && checkMember proof
    Burn        -> True
    where
        -- checks that the thread token is burned and one ref and one user NFT are minted
        checkValues :: Bool
        checkValues = ownPolMint == insert (TokenName (prefixNFT params <> pkh)) 1 (singleton (TokenName (prefixRef params <> pkh)) 1) &&
                      maybe False (\x -> x== -1) (lookup (TokenName pkh) threadPolMint)

        -- checks that the reference token is send to the predetermined locking address
        checkDatumAddr :: Bool
        checkDatumAddr = lockAddress params == txOutAddress refUtxo

        checkMember :: MT.Proof -> Bool
        checkMember proof = MT.member (pkh <> refDatumHash) (merkleRoot params) proof

        -- initialize some values that we will use --
        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        -- the currency symbol of this minting script
        ownCur :: CurrencySymbol
        ownCur = ownCurrencySymbol ctx

        -- get the values of the thread token policy and the policy of this minting script from the mint value in the context
        (threadPolMint,res1) = splitValue (threadSymbol params) (txInfoMint txInfo)
        (ownPolMint,_) = splitValue ownCur res1

        -- extract the pkh of of the participant from the tokenname of the thread token (this also checks that only one token is burned under this thread policy)
        pkh :: BuiltinByteString
        pkh = (\(x:xs)-> if xs == [] then unTokenName x else error ()) (keys threadPolMint)

        -- the TxOut that contains the ref NFT
        refUtxo :: TxOut
        refUtxo = maybe (error ()) (\x -> x) (find filter (txInfoOutputs txInfo))
            where
                filter :: TxOut -> Bool
                filter output = maybe False (\x -> x) $ member (TokenName (prefixRef params <> pkh)) <$> lookup ownCur (getValue (txOutValue output))

        refDatumHash :: BuiltinByteString
        refDatumHash = fetchDatum refUtxo
            where
                fetchDatum :: TxOut -> BuiltinByteString
                fetchDatum TxOut{txOutDatum} = case txOutDatum of
                    NoOutputDatum                   -> error ()
                    OutputDatumHash (DatumHash h)   -> if isJust (findDatum (DatumHash h) txInfo) then h else error ()
                    OutputDatum d                   -> error ()

{-# INLINABLE splitValue #-}
-- A function that given a value and a currency symbol, returns the total value under that currency symbol and the residual value in a tuple.
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
saveNFTPolicy = writeCodeToFile "Mint/assets/nft.plutus" nftCode

---------------------- A testing policy for membership proofs using merkle tree -------------------

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

savePolicy :: IO ()
savePolicy = writeCodeToFile "Mint/assets/policy.plutus" policyCode


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
saveFreePolicy = writePolicyToFile "Mint/assets/free.plutus" freePolicy

freeCurrencySymbol :: CurrencySymbol
freeCurrencySymbol = currencySymbol freePolicy

--------------------- The always fail validator -------------
{-# INLINABLE  mkAlwaysFail #-}
mkAlwaysFail :: () -> () -> ScriptContext -> Bool
mkAlwaysFail _dat _red _ctx = False

{-# INLINABLE  mkWrappedAlwaysFail #-}
mkWrappedAlwaysFail :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedAlwaysFail = wrapValidator $ mkAlwaysFail

alwaysFail :: Validator
alwaysFail = mkValidatorScript $$(compile [|| mkWrappedAlwaysFail ||])

saveAlwaysFail :: IO ()
saveAlwaysFail = writeValidatorToFile "Mint/assets/AlwaysFail.plutus" alwaysFail