{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Minting (mintingDUSDPolicy, MintParams (..), MintRedeemer (..)) where

import Plutarch
import Plutarch.Num ((#*))
import Plutarch.Api.V2 
import Plutarch.Prelude
import Plutarch.Monadic qualified as P
import Plutarch.Builtin (pasInt)
import Plutarch.Integer
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import PlutusLedgerApi.V1
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr
import Utils
import Collateral qualified
import PriceFeedOracle qualified
import Plutarch.Num (pnegate)
import PlutusTx qualified

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

-- Plutarch Data Types
data PMintParams (s :: S) =
    PMintParams (Term s (PDataRecord 
        '[
            "mpOracleNFTCurrencySymbol" ':= PCurrencySymbol
          , "mpOracleNFTTokenName"      ':= PTokenName
          , "mpOracleValidator"         ':= PScriptHash
          , "mpCollateralValidator"     ':= PScriptHash
          , "mpCollateralMinPercent"    ':= PInteger
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PMintParams where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMintParams
instance PTryFrom PData (PAsData PMintParams)

data PMintRedeemer (s :: S) = PMint | PBurn | PLiquidate
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMintRedeemer where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData PMintRedeemer)

-- Plutus Data Types
data MintParams = MintParams
    { mpOracleNFTCurrencySymbol :: CurrencySymbol
    , mpOracleNFTTokenName      :: TokenName
    , mpOracleValidator         :: ScriptHash
    , mpCollateralValidator     :: ScriptHash
    , mpCollateralMinPercent    :: Integer
    }
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''MintParams [('MintParams, 0)]

data MintRedeemer = Mint | Burn | Liquidate
    deriving stock (Generic, Show, Prelude.Eq)

--PlutusTx.makeIsDataIndexed ''MintRedeemer [('Mint, 0), ('Burn, 1), ('Liquidate, 2)]
PlutusTx.unstableMakeIsData ''MintRedeemer

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: MINTING SCRIPT --------------------------------------------

mintingDUSDPolicyT :: Term s (PMintParams :--> PMintRedeemer :--> PScriptContext :--> POpaque)
mintingDUSDPolicyT = phoistAcyclic $ plam $ \mintingParams redeemer ctx -> unTermCont $ do
    --ptraceC "Enter mintingDUSDPolicyT"
    --------------------------- General variables ---------------------------
    mintingParamsFields         <- pletFieldsC @'["mpOracleNFTCurrencySymbol", "mpOracleNFTTokenName", "mpOracleValidator", "mpCollateralValidator", "mpCollateralMinPercent"] mintingParams
    ctxFields                   <- pletFieldsC @'["txInfo", "purpose"] ctx
    ctxInfoFields               <- pletFieldsC @'["inputs", "referenceInputs", "outputs", "mint", "signatories", "datums"] ctxFields.txInfo
    PMinting purpose            <- pmatchC ctxFields.purpose
    let ownCurrencySymbol       = pfield @"_0" # purpose
    mintedItems                 <- pletC (ptryLookupValue # ownCurrencySymbol # ctxInfoFields.mint)
    mintedTokenPair             <- pletC (phead # mintedItems)
    let _mintedTokenName        = pfromData $ pfstBuiltin # mintedTokenPair
        mintedTokenAmount       = pfromData $ psndBuiltin # mintedTokenPair
        txOutputs               :: Term _ (PBuiltinList PTxOut) 
        txOutputs               = ctxInfoFields.outputs
        txInputs                :: Term _ (PBuiltinList PTxInInfo)
        txInputs                = ctxInfoFields.inputs
        txRefInputs             :: Term _ (PBuiltinList PTxInInfo)
        txRefInputs             = ctxInfoFields.referenceInputs
        priceFeedInputs         = pfilter @PBuiltinList
                                    # plam (\input -> unTermCont $ do
                                        let inputTxOutValue = pfield @"value" # (pfield @"resolved" # input)
                                            inputTxOutAddr  = pfield @"address" # (pfield @"resolved" # input)
                                            amountOracleNFT = pvalueOf # inputTxOutValue # mintingParamsFields.mpOracleNFTCurrencySymbol # mintingParamsFields.mpOracleNFTTokenName
                                        --ptraceC "amountOracleNFTs"
                                        --ptraceC $ pshow amountOracleNFT
                                        let oracleValidatorCheck = pmatch (pfield @"credential" # inputTxOutAddr) $ \case
                                                PScriptCredential vh -> (pfield @"_0" # vh) #== mintingParamsFields.mpOracleValidator
                                                _                    -> pconstant False
                                        --ptraceC "oracleValidatorCheck"
                                        --ptraceC $ pshow oracleValidatorCheck
                                        pure $ pand'List [amountOracleNFT #== 1, oracleValidatorCheck]
                                    ) # txRefInputs
        priceFeedInput          = pheadSingleton #$ priceFeedInputs
    --ptraceC "priceFeedInput success"
    --ptraceC $ pshow priceFeedInput  
    POutputDatum pfDatum        <- pmatchC(pfield @"datum" # (pfield @"resolved" # priceFeedInput))
    --ptraceC "POutputDatum success"
    let adaRateDatum            = pfromPDatum @PriceFeedOracle.POracleDatum # (pfield @"outputDatum" # pfDatum) 
        adaRate                 :: Term _ PInteger
        adaRate                 = pfield @"oADAPriceUSD" # adaRateDatum
    --ptraceC "adaRate success"
    --ptraceC $ pshow adaRate    
    --------------------------------------------------------------------------

    pure $ pif ( pmatch redeemer $ \case 
            PMint -> unTermCont $ do
                --ptraceC "enter PMint"
                let checkMintPositive       = mintedTokenAmount #> 0
                    checkMintingOutputs     = pany @PBuiltinList
                                            # plam (\txo -> unTermCont $ do
                                                --ptraceC "enter checkMintingOutputs"
                                                txOutFields             <- pletFieldsC @'["value", "address", "datum"] txo
                                                --ptraceC "txOutFields ok"
                                                --ptraceC $ pshow txOutFields.address
                                                PScriptCredential vh    <- pmatchC (pfield @"credential" # txOutFields.address)
                                                --ptraceC "PScriptCredential ok"
                                                POutputDatum dat        <- pmatchC(txOutFields.datum)
                                                --ptraceC "POutputDatum ok"
                                                let collOutputAmount    = pvalueOf # txOutFields.value # padaSymbol # padaToken
                                                    minOutputADA        = pdiv # (collOutputAmount #* 100) # mintingParamsFields.mpCollateralMinPercent
                                                    maxMintOutputAmount = pdiv # (minOutputADA #* 100) # adaRate
                                                    checkValidatorHash  = (pfield @"_0" # vh) #== mintingParamsFields.mpCollateralValidator
                                                    checkMaxMint        = maxMintOutputAmount #>= mintedTokenAmount
                                                    collDatum           = pfromPDatum @Collateral.PCollateralDatum # (pfield @"outputDatum" # dat)
                                                --ptraceC "mintingParamsFields.mpCollateralMinPercent"
                                                --ptraceC $ (pshow mintingParamsFields.mpCollateralMinPercent)
                                                --ptraceC "collOutputAmount"
                                                --ptraceC $ (pshow collOutputAmount)
                                                --ptraceC "minOutputADA"
                                                --ptraceC $ (pshow minOutputADA)
                                                --ptraceC "maxMintOutputAmount"
                                                --ptraceC $ (pshow maxMintOutputAmount)
                                                --ptraceC "checkMaxMint"
                                                --ptraceC $ (pshow checkMaxMint)
                                                --ptraceC "beforeCollDatumFields"
                                                collDatumFields         <- pletFieldsC @'["colMintingPolicyId", "colOwner", "colStablecoinAmount"] collDatum
                                                let checkCurrencySymbol = collDatumFields.colMintingPolicyId #== ownCurrencySymbol
                                                    checkMintedAmount   = collDatumFields.colStablecoinAmount #== mintedTokenAmount
                                                    checkOwnerSignature = pelem # collDatumFields.colOwner # ctxInfoFields.signatories
                                                
                                                --ptraceC "checkValidatorHash"
                                                --ptraceC $ pshow $ checkValidatorHash
                                                --ptraceC "checkMaxMint"
                                                --ptraceC $ pshow $ checkMaxMint
                                                --ptraceC "checkCurrencySymbol"
                                                --ptraceC $ pshow $ checkCurrencySymbol
                                                --ptraceC "checkMintedAmount"
                                                --ptraceC $ pshow $ checkMintedAmount
                                                --ptraceC "checkOwnerSignature"
                                                --ptraceC $ pshow $ checkOwnerSignature

                                                pure $ pand'List [checkValidatorHash, checkMaxMint, checkCurrencySymbol, checkMintedAmount, checkOwnerSignature]
                                            )
                                            # txOutputs
                --ptraceC "checkMintingOutputs success"
                --ptraceC $ pshow checkMintingOutputs  
                pure $ pand'List [checkMintPositive, checkMintingOutputs]
            _   -> unTermCont $ do
                let collInputs              = pfilter @PBuiltinList
                                            # plam (\input -> unTermCont $ do
                                                let inputTxOutAddr  = pfield @"address" # (pfield @"resolved" # input)
                                                pure $ pmatch (pfield @"credential" # inputTxOutAddr) $ \case
                                                    PScriptCredential vh -> (pfield @"_0" # vh) #== mintingParamsFields.mpCollateralValidator
                                                    _                    -> pconstant False
                                            ) # txInputs
                    collInput               = pheadSingleton #$ collInputs
                POutputDatum outputDatum    <- pmatchC(pfield @"datum" # (pfield @"resolved" # collInput))
                let collDatum               = pfromPDatum @Collateral.PCollateralDatum # (pfield @"outputDatum" # outputDatum)
                collDatumFields             <- pletFieldsC @'["colMintingPolicyId", "colOwner", "colStablecoinAmount"] collDatum
                let checkBurnNegative       = mintedTokenAmount #< 0
                    checkBurnMatchesColl    = (pnegate # collDatumFields.colStablecoinAmount) #== mintedTokenAmount
                    checkColOwner           = pelem # collDatumFields.colOwner # ctxInfoFields.signatories
                    checks                  = pmatch redeemer $ \case
                                                PBurn       ->  pand'List [checkBurnMatchesColl, checkColOwner, checkBurnNegative]
                                                PLiquidate  ->  unTermCont $ do
                                                                let collInputValue          = pfield @"value" # (pfield @"resolved" # collInput)
                                                                    collInputAmount         = pvalueOf # collInputValue # padaSymbol # padaToken
                                                                    collMinInputLovelace    = pdiv # (collInputAmount #* 100) # mintingParamsFields.mpCollateralMinPercent
                                                                    collMaxMintInputAmount  = pdiv # (collMinInputLovelace #* adaRate) # 100
                                                                    negMintedTokenAmount    = pnegate # mintedTokenAmount
                                                                    mintedAmountToLovelace  = negMintedTokenAmount #* 1000000
                                                                    checkLiquidation        = collMaxMintInputAmount #< mintedAmountToLovelace
                                                                --ptraceC "collInputAmount"
                                                                --ptraceC $ pshow $ collInputAmount
                                                                --ptraceC "collMaxMintInputAmount"
                                                                --ptraceC $ pshow $ collMaxMintInputAmount
                                                                --ptraceC "negMintedTokenAmount"
                                                                --ptraceC $ pshow $ negMintedTokenAmount
                                                                --ptraceC "mintedAmountToLovelace"
                                                                --ptraceC $ pshow $ mintedAmountToLovelace
                                                                --ptraceC "checkBurnMatchesColl"
                                                                --ptraceC $ pshow $ checkBurnMatchesColl
                                                                --ptraceC "checkLiquidation"
                                                                --ptraceC $ pshow $ checkLiquidation
                                                                --ptraceC "checkBurnNegative"
                                                                --ptraceC $ pshow $ checkBurnNegative
                                                                
                                                                pure $ pand'List [checkBurnMatchesColl, checkLiquidation, checkBurnNegative]
                                                _           -> pconstant False
                pure $ checks
        )
        (popaque (pconstant ()))
        perror

mintingDUSDPolicy :: ClosedTerm (PData :--> PMintingPolicy)
mintingDUSDPolicy = plam $ \params redeemer ctx -> unTermCont $ do
    --ptraceC "Enter mintingDUSDPolicy"
    (par,_)   <- ptryFromC @PMintParams params
    --ptraceC "params success"
    (redmr,_) <- ptryFromC @(PAsData PMintRedeemer) redeemer
    --ptraceC "redeemer success"
    pure $ popaque $ mintingDUSDPolicyT # par # pfromData redmr # ctx
    --pure $ popaque $ (pconstant ())

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS --------------------------------------------

pparseCollateralDatum :: Term s (POutputDatum :--> PMap 'Unsorted PDatumHash PDatum :--> Collateral.PCollateralDatum)
pparseCollateralDatum = phoistAcyclic $ plam $ \outputDatum datums -> unTermCont $ do
    pure $ pmatch (outputDatum) $ \case
      PNoOutputDatum _ -> perror
      POutputDatum ((pfield @"outputDatum" #) -> datum) -> unTermCont $ do
        pure $ pfromPDatum @Collateral.PCollateralDatum # datum
      POutputDatumHash ((pfield @"datumHash" #) -> dh) -> unTermCont $ do
        PJust datum <- pmatchC (ptryParseDatum # dh # datums)
        pure $ pfromPDatum @Collateral.PCollateralDatum # datum