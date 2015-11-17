{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where


-------------------------------------------------------------------------------
import qualified Data.ByteString.Char8   as B
import           Data.Monoid
import qualified Data.Text               as T
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Web.UAParser
import           Web.UAParser.SuiteUtils
-------------------------------------------------------------------------------


main :: IO ()
main = do
  uaCases <- loadTests "firefox_user_agent_strings.yaml"
  osCases <- loadTests "additional_os_tests.yaml"
  defaultMain $ testGroup "ua-parser" [ uaTests uaCases
                                      , osTests osCases
                                      ]


-------------------------------------------------------------------------------
uaTests :: [UserAgentTestCase] -> TestTree
uaTests = testGroup "UA Parsing Tests" . map testUAParser


-------------------------------------------------------------------------------
testUAParser :: UserAgentTestCase -> TestTree
testUAParser UATC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure ("Can't produce UAResult from " <> show uatcString)
    Just UAResult{..} -> do
     assertEqual "family is same" uatcFamily uarFamily
     -- assertEqual "v1 is the same" uatcV1 uarV1
     -- assertEqual "v2 is the same" uatcV2 uarV2
     -- assertEqual "v3 is the same" uatcV3 uarV3
  where
    parsed = parseUA uatcString
    tn = T.unpack $ T.intercalate "/" ["UA Test: ", uatcFamily, m uatcV1, m uatcV2, m uatcV3]
    m x = maybe "-" id x


-------------------------------------------------------------------------------
-- OS Testing
osTests :: [OSTestCase] -> TestTree
osTests = testGroup "OS Parsing Tests" . map testOSParser


-------------------------------------------------------------------------------
testOSParser :: OSTestCase -> TestTree
testOSParser OSTC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure ("Can't produce OSResult from " <> show ostcString)
    Just r@OSResult{..} -> do
     assertEqual "family is same" ostcFamily osrFamily
     assertEqual "major is the same" ostcV1  osrV1
     assertEqual "minor is the same" ostcV2  osrV2
     assertEqual "patch is the same" ostcV3  osrV3
     assertEqual "patch_minor is the same" ostcV4 osrV4
  where
    parsed = parseOS ostcString
    tn = B.unpack ostcString <> " - " <> T.unpack summary
    summary = T.intercalate "/" [ "OS Test: "
                                , ostcFamily
                                , m ostcV1
                                , m ostcV2
                                , m ostcV3
                                , m ostcV4
                                ]
    m x = maybe "-" id x
