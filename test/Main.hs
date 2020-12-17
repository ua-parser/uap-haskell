{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where


-------------------------------------------------------------------------------
import           Control.Applicative     as A
import qualified Data.ByteString.Char8   as B
import           Data.Monoid             as M
import qualified Data.Text               as T
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

-------------------------------------------------------------------------------
import           Web.UAParser
import           Web.UAParser.SuiteUtils
-------------------------------------------------------------------------------


main :: IO ()
main = do
  uaCases <- M.mconcat A.<$> mapM loadTests ["test_resources/firefox_user_agent_strings.yaml"
                                            ,"test_resources/pgts_browser_list.yaml"
                                            ,"tests/test_ua.yaml"]
  osCases <- mconcat <$> mapM loadTests [ "test_resources/additional_os_tests.yaml"
                                        , "tests/test_os.yaml" ]
  devCases <- loadTests "tests/test_device.yaml"
  setEnv "TASTY_HIDE_SUCCESSES" "true"
  defaultMain $ testGroup "ua-parser" [ uaTests uaCases
                                      , osTests osCases
                                      , devTests devCases
                                      ]


-------------------------------------------------------------------------------
uaTests :: [UserAgentTestCase] -> TestTree
uaTests = testGroup "UA Parsing Tests" . map testUAParser


-------------------------------------------------------------------------------
testUAParser :: UserAgentTestCase -> TestTree
testUAParser UATC{..} = testCase tn $ do
  assertEqual "family is same" uatcFamily uarFamily
  assertEqual "v1 is the same" uatcV1 uarV1
  assertEqual "v2 is the same" uatcV2 uarV2
  assertEqual "v3 is the same" uatcV3 uarV3
  where
    UAResult {..} = parseUALenient uatcString
    tn = T.unpack $ T.intercalate "/" ["UA Test: ", uatcFamily, m uatcV1, m uatcV2, m uatcV3]
    m x = maybe "-" id x


-------------------------------------------------------------------------------
-- OS Testing
osTests :: [OSTestCase] -> TestTree
osTests = testGroup "OS Parsing Tests" . map testOSParser


-------------------------------------------------------------------------------
testOSParser :: OSTestCase -> TestTree
testOSParser OSTC{..} = testCase tn $ do
  assertEqual "family is same" ostcFamily osrFamily
  assertEqual "major is the same" ostcV1  osrV1
  assertEqual "minor is the same" ostcV2  osrV2
  assertEqual "patch is the same" ostcV3  osrV3
  assertEqual "patch_minor is the same" ostcV4 osrV4
  where
    OSResult {..} = parseOSLenient ostcString
    tn = B.unpack ostcString <> " - " <> T.unpack summary
    summary = T.intercalate "/" [ "OS Test: "
                                , ostcFamily
                                , m ostcV1
                                , m ostcV2
                                , m ostcV3
                                , m ostcV4
                                ]
    m x = maybe "-" id x


-------------------------------------------------------------------------------
-- Dev Testing
devTests :: [DevTestCase] -> TestTree
devTests = testGroup "Dev Parsing Tests" . map testDevParser


-------------------------------------------------------------------------------
testDevParser :: DevTestCase -> TestTree
testDevParser DTC{..} = testCase tn $ do
  assertEqual "family is same" dtcFamily drFamily
  assertEqual "brand is the same" dtcBrand drBrand
  assertEqual "model is the same" dtcModel drModel
  where
    DevResult {..} = parseDevLenient dtcString
    tn = B.unpack dtcString <> " - " <> T.unpack summary
    summary = T.intercalate "/" [ "Dev Test: "
                                , dtcFamily
                                , m dtcBrand
                                , m dtcModel
                                ]
    m x = maybe "-" id x
