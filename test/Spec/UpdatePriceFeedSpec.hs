module Spec.UpdatePriceFeedSpec (updatePriceFeedTest, updatePriceFeedCtx1) where
import PriceFeedOracle (priceFeedOracleValidator, OracleDatum (OracleDatum), OracleParams (OracleParams))
import Plutarch.Context (
    Builder,
    SpendingBuilder,
    --MintingBuilder, 
    address,
    buildSpending,
    --buildMinting,
    checkPhase1,
    fee,
    input,
    output,
    script,
    signedWith,
    txId,
    --referenceInput,
    withRedeemer,
    withRefIndex,
    withRefTxId,
    withSpendingOutRefId,
    withValue,
    -- withMinting,
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

oracleScriptHash :: ScriptHash
oracleScriptHash = "459e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

oracleAddress :: Address
oracleAddress = Address (ScriptCredential oracleScriptHash) Nothing

oracleNFTPolicyID :: CurrencySymbol
oracleNFTPolicyID = "345e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

alicePubKeyHash :: PubKeyHash
alicePubKeyHash = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

aliceAddress :: Address
aliceAddress = Address (PubKeyCredential alicePubKeyHash) Nothing

oracleNFTToken :: Value
oracleNFTToken = singleton (oracleNFTPolicyID) (TokenName "OracleNFT") (1)

inputFromOracle :: (Builder a) => a
inputFromOracle =
    input $
        mconcat
            [ script oracleScriptHash
            , withValue (oracleNFTToken <> (singleton adaSymbol adaToken 2000000))
            , withInlineDatum $ PlutusTx.toData (OracleDatum 100)
            , withRedeemer $ PlutusTx.toData (0 :: Integer) -- Redeemer
            , withRefTxId "12325f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
            , withRefIndex 1
            ]

inputFromAlice :: (Builder a) => a
inputFromAlice =
    input $
        mconcat
            [ address aliceAddress
            , withValue (singleton adaSymbol adaToken 4000000)
            , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
            , withRefIndex 2
            ]

outputToAliceADA :: (Builder a) => a
outputToAliceADA =
    output $
        mconcat
            [ address aliceAddress
            , withValue
                (singleton adaSymbol adaToken 2000000)
            ]

outputToOracle :: (Builder a) => a
outputToOracle =
    output $
        mconcat
            [ address oracleAddress
            , withValue (oracleNFTToken <> (singleton adaSymbol adaToken 2000000))
            , withInlineDatum $ PlutusTx.toData (OracleDatum (80 :: Integer))
            ]

commonPurpose :: SpendingBuilder
commonPurpose = withSpendingOutRefId "12325f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"

updatePriceFeedCtx1 :: ScriptContext
updatePriceFeedCtx1 =
    buildSpending checkPhase1 $
        mconcat
            [ inputFromOracle
            , inputFromAlice
            , outputToOracle
            , outputToAliceADA
            , signedWith alicePubKeyHash
            , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
            , fee (singleton adaSymbol adaToken 2000000)
            , commonPurpose
            ]

updatePriceFeedTest :: TestTree
updatePriceFeedTest = tryFromPTerm "Test init PriceFeedOracle" priceFeedOracleValidator $ do
    testEvalCase
        "Pass "
        Success
        [ PlutusTx.toData (OracleParams oracleNFTPolicyID (TokenName "OracleNFT") alicePubKeyHash) --- Param
        , PlutusTx.toData ((OracleDatum 80)) -- Datum
        , PlutusTx.toData (0 :: Integer) -- Redeemer
        , PlutusTx.toData updatePriceFeedCtx1 -- ScriptContext -> tx
        ]
