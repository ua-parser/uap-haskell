{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import           Control.DeepSeq
import           Criterion.Main                 as C
import           Data.Aeson
import           Data.ByteString.Char8          (ByteString)
import           Data.DeriveTH
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Yaml
import           System.Environment
import           System.FilePath.Posix
import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework                 as T
import           Test.Framework.Providers.HUnit as T
import           Test.HUnit                     hiding (Test, path)

import           Web.UAParser


main :: IO ()
main = do
  arg <- getArgs
  case arg of
    ["bench"] -> benchMain
    _ -> testMain



benchMain :: IO ()
benchMain = do
  cases <- loadTests "firefox_user_agent_strings.yaml"
  let ua = bench "UA Parsing" $ nf (map (parseUA . uatcString)) cases
  print $ show (length cases) ++ " strings being parsed."
  C.defaultMain [ua]



testMain :: IO ()
testMain = T.defaultMain tests
    where
      tests =
        [ uaTests
        , osTests ]


uaTests :: Test
uaTests = buildTest $ do
  cases <- loadTests "firefox_user_agent_strings.yaml"
  return $ testGroup "UA Parsing Tests" $ map testUAParser cases



testUAParser :: UserAgentTestCase -> Test
testUAParser UATC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure "Can't produce UAResult"
    Just UAResult{..} -> do
     assertEqual "family is same" uatcFamily uarFamily
     -- assertEqual "v1 is the same" uatcV1 uarV1
     -- assertEqual "v2 is the same" uatcV2 uarV2
     -- assertEqual "v3 is the same" uatcV3 uarV3
  where
    parsed = parseUA uatcString
    tn = T.unpack $ T.intercalate "/" ["UA Test: ", uatcFamily, m uatcV1, m uatcV2, m uatcV3]
    m x = maybe "-" id x


-- OS Testing
osTests :: Test
osTests = buildTest $ do
  cases <- loadTests "additional_os_tests.yaml"
  return $ testGroup "OS Parsing Tests" $ map testOSParser cases



testOSParser :: OSTestCase -> Test
testOSParser OSTC{..} = testCase tn $ do
  case parsed of
    Nothing -> assertFailure "Can't produce OSResult"
    Just OSResult{..} -> do
     assertEqual "family is same" ostcFamily osrFamily
     assertEqual "major is the same" ostcV1  osrV1
     assertEqual "minor is the same" ostcV2  osrV2
     assertEqual "patch is the same" ostcV3  osrV3
     assertEqual "patch_minor is the same" ostcV4 osrV4
  where
    parsed = parseOS ostcString
    tn = T.unpack $ T.intercalate "/"
         ["OS Test: ", ostcFamily, m ostcV1, m ostcV2, m ostcV3, m ostcV4]
    m x = maybe "-" id x





-- Loading Test Cases

loadTests :: FromJSON a => FilePath -> IO a
loadTests fp = parseMonad p =<< either (error . show) id `fmap` decodeFileEither fp'
  where
    fp' = "uap-core/test_resources" </> fp
    p (Object x) = x .: "test_cases"


data UserAgentTestCase = UATC {
      uatcString :: ByteString
    , uatcFamily :: Text
    , uatcV1     :: Maybe Text
    , uatcV2     :: Maybe Text
    , uatcV3     :: Maybe Text
    } deriving (Show)


instance FromJSON UserAgentTestCase where
    parseJSON (Object v) =
      UATC <$> T.encodeUtf8 <$> v .: "user_agent_string"
           <*> (v .: "family" <|> return "")
           <*> (v .:? "v1" <|> return Nothing)
           <*> (v .:? "v2" <|> return Nothing)
           <*> (v .:? "v3" <|> return Nothing)


data OSTestCase = OSTC {
      ostcString :: ByteString
    , ostcFamily :: Text
    , ostcV1     :: Maybe Text
    , ostcV2     :: Maybe Text
    , ostcV3     :: Maybe Text
    , ostcV4     :: Maybe Text
    } deriving (Show)


instance FromJSON OSTestCase where
    parseJSON (Object v) =
      OSTC <$> (T.encodeUtf8 <$> v .: "user_agent_string" <|> return "")
           <*> (v .: "family" <|> return "")
           <*> nonBlank (v .:? "major" <|> return Nothing)
           <*> nonBlank (v .:? "minor" <|> return Nothing)
           <*> nonBlank (v .:? "patch" <|> return Nothing)
           <*> nonBlank (v .:? "patch_minor" <|> return Nothing)

nonBlank :: (Monad m) =>
            m (Maybe Text) -> m (Maybe Text)
nonBlank f = do
  res <- f
  return $ case res of
    Just "" -> Nothing
    Just x -> Just x
    Nothing -> Nothing



$(derives [makeNFData] [''UAResult])
