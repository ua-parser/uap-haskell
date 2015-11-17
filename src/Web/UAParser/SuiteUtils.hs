{-# LANGUAGE OverloadedStrings #-}
module Web.UAParser.SuiteUtils
    ( UserAgentTestCase(..)
    , OSTestCase(..)
    , loadTests
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as T
import           Data.Yaml
import           System.FilePath
-------------------------------------------------------------------------------


-- Loading Test Cases

loadTests :: FromJSON a => FilePath -> IO a
loadTests fp = parseMonad p =<< either (error . show) id `fmap` decodeFileEither fp'
  where
    fp' = "deps/uap-core/test_resources" </> fp
    p = withObject "Value" $ \x -> x .: "test_cases"

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
  parseJSON = withObject "OSTestCase" parse
    where parse v = OSTC <$> (T.encodeUtf8 <$> v .: "user_agent_string" <|> return "")
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



