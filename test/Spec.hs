module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.MintingSpec         (mintTest)
import Spec.BurningSpec         (burnTest)
import Spec.LiquidationSpec     (liquidationTest)
import Spec.OracleNFTSpec       (mintOracleNFTTest)
import Spec.UpdatePriceFeedSpec (updatePriceFeedTest)

main :: IO ()
main = do
    defaultMain $
        testGroup
            "All Tests"
            [ 
             unitTests
            ]

-- Unit Testing
unitTests :: TestTree
unitTests =
    testGroup
        "All Unit Tests"
        [ 
            mintTest
          , burnTest
          , liquidationTest
          , mintOracleNFTTest
          , updatePriceFeedTest
        ]
