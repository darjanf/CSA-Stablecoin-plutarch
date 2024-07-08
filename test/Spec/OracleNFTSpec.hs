module Spec.OracleNFTSpec (mintOracleNFTTest, mintOracleNFTCtx1) where
import OracleNFT (oracleNFTPolicy, OracleMintParams(OracleMintParams))
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
    --script,
    signedWith,
    txId,
    --referenceInput,
    --withRedeemer,
    withRefIndex,
    withRefTxId,
--    withSpendingOutRefIdx,
    withValue,
    withMinting,
    --withInlineDatum
 )
import Plutarch.Test.Precompiled (Expectation (Success), testEvalCase, tryFromPTerm)
import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2
import PlutusTx qualified
import Test.Tasty (TestTree)

-- unit test contracts -> here - pass
-- property test contracts or functions

-- unit test offchain
-- property test offhchain
-- integration test offchain
-- stress test offchain

oracleNFTPolicyID :: CurrencySymbol
oracleNFTPolicyID = "345e0b6c308dbdfd6e41354b68f833b96990ecd93721699ed90a2113"

alicePubKeyHash :: PubKeyHash
alicePubKeyHash = "0d342d962a7aaac57e30d3f8dd2f41907a361860f8889253ebe40bbb"

{- samplePubKeyHash2 :: PubKeyHash
samplePubKeyHash2 = "ea2484f839e72f5bd60e004e74b564bb75f79a980b22c55d88f4b8bb" -}

aliceAddress :: Address
aliceAddress = Address (PubKeyCredential alicePubKeyHash) Nothing

aliceTxOutRef :: TxOutRef
aliceTxOutRef = TxOutRef ("24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608") (2)

oracleNFTToken :: Value
oracleNFTToken = singleton (oracleNFTPolicyID) (tokenName "OracleNFT") (1)

inputFromAlice :: (Builder a) => a
inputFromAlice =
    input $
        mconcat
            [ address aliceAddress
            , withValue (singleton adaSymbol adaToken 10000000)
            , withRefTxId "24625f40313747ed839c2e20de5c1e2040c01411e6f528ee4b4abae5115c6608"
            , withRefIndex 2
            ]
            
outputToAliceOracleNFT :: (Builder a) => a
outputToAliceOracleNFT =
    output $
        mconcat
            [ address aliceAddress
            , withValue
                (oracleNFTToken <> (singleton adaSymbol adaToken 8000000))
            ]

commonPurpose :: MintingBuilder
commonPurpose = withMinting oracleNFTPolicyID

mintOracleNFTCtx1 :: ScriptContext
mintOracleNFTCtx1 =
    buildMinting checkPhase1 $
        mconcat
            [ inputFromAlice
            , outputToAliceOracleNFT
            , mint oracleNFTToken
            , signedWith alicePubKeyHash
            , txId "b2dfbe34017b9061464f401ec924ece385bb3ec07061c27907844b4d3ef6666e"
            , fee (singleton adaSymbol adaToken 2000000)
            , commonPurpose
            ]

mintOracleNFTTest :: TestTree
mintOracleNFTTest = tryFromPTerm "Test Minting OracleNFT" oracleNFTPolicy $ do
    testEvalCase
        "Pass "
        Success
        [ PlutusTx.toData (OracleMintParams aliceTxOutRef (TokenName "OracleNFT"))   --- Param
        , PlutusTx.toData (0 :: Integer)    -- Redeemer
        , PlutusTx.toData mintOracleNFTCtx1 -- ScriptContext -> tx
        ]
