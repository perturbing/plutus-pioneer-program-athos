{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Minting where

import           Plutus.V2.Ledger.Api (BuiltinData, PubKeyHash,
                                       ScriptContext,CurrencySymbol,
                                       MintingPolicy, mkMintingPolicyScript,
                                       mkValidatorScript, Validator, Address,
                                       ScriptContext (..), TxInfo (..),TxInInfo (..))
import           PlutusTx             (compile, makeIsDataIndexed, CompiledCode, unsafeFromBuiltinData)
import           PlutusTx.Prelude     (Bool (..),BuiltinByteString,(==),($),length)
import           Utilities            (wrapPolicy, writeCodeToFile,writePolicyToFile,currencySymbol,
                                      writeValidatorToFile, wrapValidator)
import           Prelude              (IO)
import qualified Plutus.MerkleTree    as MT

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / MINTING-VALIDATOR ----------------------------------

-- | The minting policy that upon burning of the thread token + a merkle proof of the datum, mints the PPP NFT.

data Parameters = Parameters {
      merkleRoot    :: MT.Hash             -- the merkle root of the NFT metadata (member = pubkeyhash <> datumHash)
    , prefix1       :: BuiltinByteString   -- the (222) nft prefix 
    , prefix2       :: BuiltinByteString   -- the (100) ref prefix 
    , threadSymbol  :: CurrencySymbol      -- the currency symbol of the thread token
--    , lockAddres    :: Address             -- the address to lock the reference NFT at
}
makeIsDataIndexed ''Parameters [('Parameters,0)]

{-# INLINABLE mkNFTPolicy #-}
-- mkNFTPolicy :: Parameters -> MT.Proof -> ScriptContext -> Bool
mkNFTPolicy :: Parameters -> MT.Hash -> ScriptContext -> Bool
mkNFTPolicy params proof _ctx = prefix1 params == prefix2 params

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