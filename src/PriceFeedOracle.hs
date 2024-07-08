{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module PriceFeedOracle (priceFeedOracleValidator, POracleDatum (..), OracleParams (..), OracleDatum (..)) where

import Plutarch 
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Prelude 
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import PlutusLedgerApi.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr
import Plutarch.Extra.Api (pfindOwnInput, pparseDatum)
import Utils
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Builtin (pasInt)
import PlutusTx qualified
---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

data POracleParams (s :: S) =
    POracleParams (Term s (PDataRecord 
        '[
            "oNFTCurrencySymbol"    ':= PCurrencySymbol
          , "oNFTTokenName"         ':= PTokenName
          , "oOperator"             ':= PPubKeyHash
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType POracleParams where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POracleParams
instance PTryFrom PData (PAsData POracleParams)

data POracleRedeemer (s :: S) = PUpdate | PDelete
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType POracleRedeemer where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData POracleRedeemer)

-- Oracle Datum
data POracleDatum (s :: S) =
    POracleDatum (Term s (PDataRecord 
        '[
            "oADAPriceUSD"    ':= PInteger
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType POracleDatum where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POracleDatum
instance PTryFrom PData (PAsData POracleDatum)

-- Plutus Data Types
data OracleParams = OracleParams
    { 
        oNFTCurrencySymbol    :: CurrencySymbol
      , oNFTTokenName         :: TokenName
      , oOperator             :: PubKeyHash
    }
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''OracleParams [('OracleParams, 0)]

data OracleDatum = OracleDatum
    { 
        oADAPriceUSD :: Integer
    }
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''OracleDatum [('OracleDatum, 0)]

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: VALIDATOR -------------------------------------------------
priceFeedOracleValidatorT :: Term s (POracleParams :--> POracleDatum :--> POracleRedeemer :--> PScriptContext :--> POpaque)
priceFeedOracleValidatorT = phoistAcyclic $ plam $ \params datum redeemer ctx -> unTermCont $ do
    -- ptraceC "Enter PriceFeedOracleT"
    
    ctxFields                                   <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfoFields                                <- pletFieldsC @'["signatories", "inputs", "outputs", "datums"] ctxFields.txInfo
    paramsFields                                <- pletFieldsC @'["oNFTCurrencySymbol", "oNFTTokenName", "oOperator"] params
    let _adaPrice                               = pfield @"oADAPriceUSD" # datum
    -- ptraceC "after adaPrice"
    PSpending ((pfield @"_0" #) -> txOutRef)    <- pmatchC (ctxFields.purpose)
    -- ptraceC "after spending"
    PTxOut ownInput                             <- pmatchC (ptryOwnInput # txInfoFields.inputs # txOutRef)
    PTxOut ownOutput                            <- pmatchC (ptryGetContinuingOutput # txInfoFields.inputs # txInfoFields.outputs # txOutRef)
    -- ptraceC "after ownOutput"
    -- ptraceC $ pshow $ ownOutput
    let ownInputValue                           = pfield @"value" # ownInput
        ownOutputValue                          = pfield @"value" # ownOutput
        inputNFTAmount                          = pvalueOf # ownInputValue # paramsFields.oNFTCurrencySymbol # paramsFields.oNFTTokenName
        outputNFTAmount                         = pvalueOf # ownOutputValue # paramsFields.oNFTCurrencySymbol # paramsFields.oNFTTokenName
        inputHasToken                           = inputNFTAmount #== pconstant 1
        outputHasToken                          = outputNFTAmount #== pconstant 1
        checkOperatorSignature                  = pelem # paramsFields.oOperator # txInfoFields.signatories
        parsedOracleDatum                       = ptryParseOracleDatum # (pcon $ PTxOut ownOutput) # txInfoFields.datums
    -- ptraceC "after ptryParseOracleDatum"
    PJust validOutputDatum                      <- pmatchC (parsedOracleDatum)
    -- ptraceC "inputHasToken"
    -- ptraceC $ pshow $ inputHasToken
    -- ptraceC "outputHasToken"
    -- ptraceC $ pshow $ outputHasToken
    -- ptraceC "checkOperatorSignature"
    -- ptraceC $ pshow $ checkOperatorSignature
    -- ptraceC "validOutputDatum"
    -- ptraceC $ pshow $ validOutputDatum

    pure $ pif ( pmatch redeemer $ \case 
            PUpdate ->
                pand'List [   
                              ptraceIfFalse "token missing from input"   inputHasToken
                            , ptraceIfFalse "token missing from output"  outputHasToken
                            , ptraceIfFalse "operator signature missing" checkOperatorSignature
                            , ptraceIfFalse "invalid output datum"       (validOutputDatum #> pconstant 0)
                          ]
            PDelete ->
                pand'List [ 
                    ptraceIfFalse "operator signature missing" checkOperatorSignature
                    ]
        )
        (popaque (pconstant ()))
        perror

priceFeedOracleValidator :: ClosedTerm (PData :--> PValidator)
priceFeedOracleValidator = plam $ \params datum redeemer ctx -> unTermCont $ do
  -- ptraceC "Enter PriceFeedOracleT"
  (par,_)   <- ptryFromC @(PAsData POracleParams) params
  -- ptraceC "after params"
  (dat,_)   <- ptryFromC @(PAsData POracleDatum) datum
  -- ptraceC "after datum"
  (redmr,_) <- ptryFromC @(PAsData POracleRedeemer) redeemer
  -- ptraceC "after redeemer"
  pure $ popaque $ priceFeedOracleValidatorT # pfromData par # pfromData dat # pfromData redmr # ctx

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTION -------------------------------------------------

ptryParseOracleDatum :: Term s (PTxOut :--> PMap 'Unsorted PDatumHash PDatum :--> PMaybe PInteger)
ptryParseOracleDatum = phoistAcyclic $ plam $ \output datums -> unTermCont $ do
    pure $ pmatch (pfield @"datum" # output) $ \case
        PNoOutputDatum _ -> pcon $ PNothing
        POutputDatum outputDatum -> unTermCont $ do
            let adaRateDatum        = pfromPDatum @POracleDatum # (pfield @"outputDatum" # outputDatum)
                adaRate             :: Term _ PInteger
                adaRate             = pfield @"oADAPriceUSD" # adaRateDatum
            --PDatum d <- pmatchC(datum)
            -- ptraceC "adaRate"
            -- ptraceC $ pshow $ (adaRate)
            pure $ pcon $ PJust (adaRate)
        POutputDatumHash ((pfield @"datumHash" #) -> dh) -> unTermCont $ do
            PJust datum <- pmatchC (ptryParseDatum # dh # datums)
            let adaRateDatum        = pfromPDatum @POracleDatum # (datum)
                adaRate             :: Term _ PInteger
                adaRate             = pfield @"oADAPriceUSD" # adaRateDatum
            --PDatum d <- pmatchC (datum)
            pure $ pcon $ PJust (adaRate)