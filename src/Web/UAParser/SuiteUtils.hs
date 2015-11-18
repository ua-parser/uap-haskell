{-# LANGUAGE OverloadedStrings #-}
module Web.UAParser.SuiteUtils
    ( UserAgentTestCase(..)
    , OSTestCase(..)
    , DevTestCase(..)
    , loadTests
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           Data.Yaml
import           System.FilePath
-------------------------------------------------------------------------------


-- Loading Test Cases
loadTests :: FromJSON a => FilePath -> IO a
loadTests fp = parseMonad p =<< either (error . show) id `fmap` decodeFileEither fp'
  where
    fp' = "deps/uap-core" </> fp
    p = withObject "Value" $ \x -> x .: "test_cases"


-------------------------------------------------------------------------------
data UserAgentTestCase = UATC {
      uatcString :: ByteString
    , uatcFamily :: Text
    , uatcV1     :: Maybe Text
    , uatcV2     :: Maybe Text
    , uatcV3     :: Maybe Text
    } deriving (Show)


instance FromJSON UserAgentTestCase where
  parseJSON = withObject "UserAgentTestCase" parse
    where parse v = UATC <$> T.encodeUtf8 <$> v .: "user_agent_string"
                         <*> v .: "family"
                         <*> v .:? "major"
                         <*> v .:? "minor"
                         <*> v .:? "patch"


-------------------------------------------------------------------------------
data OSTestCase = OSTC {
      ostcString :: ByteString
    , ostcFamily :: Text
    , ostcV1     :: Maybe Text
    , ostcV2     :: Maybe Text
    , ostcV3     :: Maybe Text
    , ostcV4     :: Maybe Text
    } deriving (Show)


instance FromJSON OSTestCase where
  parseJSON = withObject "OSTestCase" parse
    where parse v = OSTC <$> (T.encodeUtf8 <$> v .: "user_agent_string" <|> return "")
                         <*> v .: "family"
                         <*> nonBlank (v .:? "major")
                         <*> nonBlank (v .:? "minor")
                         <*> nonBlank (v .:? "patch")
                         <*> nonBlank (v .:? "patch_minor")

nonBlank :: (Monad m) =>
            m (Maybe Text) -> m (Maybe Text)
nonBlank f = do
  res <- f
  return $ case res of
    Just "" -> Nothing
    Just x -> Just x
    Nothing -> Nothing


-------------------------------------------------------------------------------
data DevTestCase = DTC {
      dtcString :: ByteString
    , dtcFamily :: Text
    , dtcBrand  :: Maybe Text
    , dtcModel  :: Maybe Text
    } deriving (Show, Eq)


instance FromJSON DevTestCase where
  parseJSON = withObject "DevTestCase" parse
    where parse o = DTC <$> (T.encodeUtf8 <$> o .: "user_agent_string")
                        <*> o .: "family"
                        <*> nonBlank (o .:? "brand")
                        <*> nonBlank (o .:? "model")
