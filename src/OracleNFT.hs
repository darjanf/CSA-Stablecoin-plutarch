{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module OracleNFT (oracleNFTPolicy, OracleMintParams(..)) where

import Plutarch 
import Plutarch.Api.V2 
import Plutarch.Prelude 
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import PlutusLedgerApi.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr
import PlutusTx qualified
import Utils

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

-- Plutus Data Types
data OracleMintParams = OracleMintParams
    { oRefTx     :: TxOutRef
    , oTokenName    :: TokenName
    }
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''OracleMintParams [('OracleMintParams, 0)]

-- Plutarch Data Types
data POracleMintParams (s :: S) =
    POracleMintParams (Term s (PDataRecord 
        '[
            "oRefTx"        ':= PTxOutRef
          , "oTokenName"    ':= PTokenName
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType POracleMintParams where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POracleMintParams
instance PTryFrom PData (PAsData POracleMintParams)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: MINTING SCRIPT --------------------------------------------

-- OracleNFT minting script
oracleNFTPolicyT :: Term s (POracleMintParams :--> PData :--> PScriptContext :--> POpaque)
oracleNFTPolicyT = phoistAcyclic $ plam $ \params _red ctx -> unTermCont $ do
    --ptraceC "Enter OracleNFT"
    ----ptraceC $ pshow $ collMaxMintInputAmount
    paramsFields            <- pletFieldsC @'["oRefTx", "oTokenName"] params
    ctxFields               <- pletFieldsC @'["txInfo", "purpose"] ctx
    ctxInfoFields           <- pletFieldsC @'["inputs", "mint"] ctxFields.txInfo
    PMinting purpose        <- pmatchC ctxFields.purpose
    -- or PMinting ((pfield @"_0" #) -> ownCurrencySymbol)
    --ptraceC "after purpose"
    let ownCurrencySymbol   = pfield @"_0" # purpose
    mintedItems             <- pletC (ptryLookupValue # ownCurrencySymbol # ctxInfoFields.mint)
    mintedTokenPair         <- pletC (phead # mintedItems)
    --ptraceC "after mintedTokenPair"
    let checkMintedAmount   = plength # mintedItems #== pconstant 1
        mintedTokenName     = pfromData $ pfstBuiltin # mintedTokenPair
        mintedTokenAmount   = pfromData $ psndBuiltin # mintedTokenPair
        checkTokenAmount    = mintedTokenAmount #== pconstant 1
    --ptraceC "after checkTokenAmount"
    --PPair tokenLabel _      <- pmatchC (pbreakTokenName mintedTokenName)
    --ptraceC "tokenLabel"
    --ptraceC $ pshow $ tokenLabel
    --ptraceC "paramsFields.oTokenName"
    --ptraceC $ pshow $ paramsFields.oTokenName
    checkTokenName          <- pletC (mintedTokenName #== paramsFields.oTokenName)
    --ptraceC "after checkTokenName"
    hasUtxo                 <- pletC (
                                pany @PBuiltinList 
                                # plam (\txIn -> let txInRef = (pfield @"outRef" # txIn) in txInRef #== paramsFields.oRefTx)
                                # ctxInfoFields.inputs
                                )
    --ptraceC "after hasUtxo"
    --ptraceC "hasUtxo"
    --ptraceC $ pshow $ hasUtxo
    --ptraceC "checkTokenName"
    --ptraceC $ pshow $ checkTokenName
    --ptraceC "checkMintedAmount"
    --ptraceC $ pshow $ checkMintedAmount
    --ptraceC "checkTokenAmount"
    --ptraceC $ pshow $ checkTokenAmount
    let checks              = pand'List [hasUtxo,checkTokenName,checkMintedAmount,checkTokenAmount]

    pure $ pif checks
        (popaque (pconstant ()))
        perror

oracleNFTPolicy :: ClosedTerm (PData :--> PMintingPolicy)
oracleNFTPolicy = plam $ \params redeemer ctx -> unTermCont $ do
    (par,_)   <- ptryFromC @POracleMintParams params
    pure $ popaque $ oracleNFTPolicyT # par # redeemer # ctx