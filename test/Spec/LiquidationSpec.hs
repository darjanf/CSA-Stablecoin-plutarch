module Spec.LiquidationSpec (liquidationTest, liquidationCtx1) where
import Minting (mintingDUSDPolicy, MintParams (MintParams))
import PriceFeedOracle (OracleDatum (OracleDatum))
import Collateral qualified
import Plutarch.Context (
    Builder,
--    SpendingBuilder,
    MintingBuilder, 
    address,
--    buildSpending,
    buildMinting,
    checkPhase1,
    fee,
    input,
    output,
    mint,
    script,
    signedWith,
    txId,
    referenceInput,
    withRedeemer,
    withRefIndex,
    withRefTxId,
--    withSpendingOutRefIdx,
    withValue,
    withMinting,
    withInlineDatum
 )
import Plutarch.Test.Precompiled (Expectation (Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V2
--import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusTx qualified
import Test.Tasty (TestTree)

-- unit test contracts -> here - pass
-- property test contracts or functions

-- unit test offchain
-- property test offhchain
-- integration test offchain
-- stress test offchain

mintingPolicyID :: CurrencySymbol
mintingPolicyID = "123e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

oracleNFTPolicyID :: CurrencySymbol
oracleNFTPolicyID = "345e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

oracleScriptHash :: ScriptHash
oracleScriptHash = "459e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

collScriptHash :: ScriptHash
collScriptHash = "789e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

alicePubKeyHash :: PubKeyHash
alicePubKeyHash = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

bobPubKeyHash :: PubKeyHash
bobPubKeyHash = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb"

{- aliceAddress :: Address
aliceAddress = Address (PubKeyCredential alicePubKeyHash) Nothing -}

bobAddress :: Address
bobAddress = Address (PubKeyCredential bobPubKeyHash) Nothing

collateralAddress :: Address
collateralAddress = Address (ScriptCredential collScriptHash) Nothing

dUSDToken :: Value
dUSDToken = singleton (mintingPolicyID) (TokenName "dUSDToken") (9)

dUSDNegToken :: Value
dUSDNegToken = singleton (mintingPolicyID) (TokenName "dUSDToken") (-9)

oracleNFTToken :: Value
oracleNFTToken = singleton (oracleNFTPolicyID) (TokenName "OracleNFT") (1)

oracleRefInput :: (Builder a) => a
oracleRefInput =
    referenceInput $
        mconcat
            [ script oracleScriptHash
            , withValue (oracleNFTToken <> (singleton adaSymbol adaToken 10000000))
            , withInlineDatum $ PlutusTx.toData (PriceFeedOracle.OracleDatum 79)
            , withRefTxId "eeff"
            , withRefIndex 2
            ]

inputFromCollateralScript :: (Builder a) => a
inputFromCollateralScript =
    input $
        mconcat
            [ address collateralAddress
            , withValue (singleton adaSymbol adaToken 16875000)
            , withInlineDatum $ PlutusTx.toData (Collateral.CollateralDatum mintingPolicyID alicePubKeyHash 9)
            , withRedeemer $ PlutusTx.toData (0 :: Integer) -- Redeemer
            , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
            , withRefIndex 2
            ]

inputFromBobDUSD :: (Builder a) => a
inputFromBobDUSD =
    input $
        mconcat
            [ address bobAddress
            , withValue (dUSDToken <> (singleton adaSymbol adaToken 2000000))
            , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
            , withRefIndex 3
            ]

outputToBobADA :: (Builder a) => a
outputToBobADA =
    output $
        mconcat
            [ address bobAddress
            , withValue (singleton adaSymbol adaToken 16875000)
            ]

commonPurpose :: MintingBuilder
commonPurpose = withMinting mintingPolicyID

liquidationCtx1 :: ScriptContext
liquidationCtx1 =
    buildMinting checkPhase1 $
        mconcat
            [ oracleRefInput
            , inputFromCollateralScript
            , inputFromBobDUSD
            , outputToBobADA
            , mint dUSDNegToken
            , signedWith bobPubKeyHash
            , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
            , fee (singleton adaSymbol adaToken 2000000)
            , commonPurpose
            ]

liquidationTest :: TestTree
liquidationTest = tryFromPTerm "Test Liquidating dUSD" mintingDUSDPolicy $ do
    testEvalCase
        "Pass "
        Success
        [ PlutusTx.toData (MintParams oracleNFTPolicyID (TokenName "OracleNFT") oracleScriptHash collScriptHash 150) --- Param
        , PlutusTx.toData (2 :: Integer) -- Redeemer
        , PlutusTx.toData liquidationCtx1 -- ScriptContext -> tx
        ]