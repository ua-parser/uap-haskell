{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Web.UAParser
    ( -- * Parsing browser (user agent)
      parseUA
    , UAResult (..)
    , uarVersion

      -- * Parsing OS
    , parseOS
    , OSResult (..)
    , osrVersion
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import           Data.Default
import           Data.FileEmbed
import           Data.Generics
import           Data.Maybe
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Yaml
import           Text.Regex.PCRE.Light
-------------------------------------------------------------------------------


                                ---------------
                                -- UA Parser --
                                ---------------

uaConfig :: UAConfig
uaConfig = either error id $ decodeEither $(embedFile "uap-core/regexes.yaml")
{-# NOINLINE uaConfig #-}

-------------------------------------------------------------------------------
-- | Parse a given User-Agent string
parseUA :: ByteString -> Maybe UAResult
parseUA bs = msum $ map go uaParsers
    where
      UAConfig{..} = uaConfig

      go UAParser{..} = either (const Nothing) mkRes
                      . mapM T.decodeUtf8' =<< match uaRegex bs []
        where
          mkRes [_,f,v1,v2,v3] = Just $ UAResult (repF f) (repV1 v1) (Just v2) (Just v3)
          mkRes [_,f,v1,v2]    = Just $ UAResult (repF f) (repV1 v1) (Just v2) Nothing
          mkRes [_,f,v1]       = Just $ UAResult (repF f) (repV1 v1) Nothing   Nothing
          mkRes [_,f]          = Just $ UAResult (repF f) Nothing    Nothing   Nothing
          mkRes _              = Nothing

          repV1 x = uaV1Rep `mplus` Just x
          repF x = maybe x id uaFamRep



-------------------------------------------------------------------------------
-- | Results datatype for the parsed User-Agent
data UAResult = UAResult {
      uarFamily :: Text
    , uarV1     :: Maybe Text
    , uarV2     :: Maybe Text
    , uarV3     :: Maybe Text
    } deriving (Show, Read, Eq, Typeable, Data)


-------------------------------------------------------------------------------
-- | Construct a browser version-string from 'UAResult'
uarVersion :: UAResult -> Text
uarVersion UAResult{..} =
    T.intercalate "." . catMaybes . takeWhile isJust $ [uarV1, uarV2, uarV3]


-------------------------------------------------------------------------------
instance Default UAResult where
    def = UAResult "" Nothing Nothing Nothing



                                ---------------
                                -- OS Parser --
                                ---------------

-------------------------------------------------------------------------------
-- | Parse OS from given User-Agent string
parseOS :: ByteString -> Maybe OSResult
parseOS bs = msum $ map go osParsers
    where
      UAConfig{..} = uaConfig

      go OSParser{..} = either (const Nothing) mkRes
                      . mapM T.decodeUtf8' =<< match osRegex bs []
          where
          mkRes [_,f,v1,v2,v3,v4] = Just $ OSResult (repF f) (Just v1) (Just v2) (Just v3) (Just v4)
          mkRes [_,f,v1,v2,v3]    = Just $ OSResult (repF f) (Just v1) (Just v2) (Just v3) Nothing
          mkRes [_,f,v1,v2]       = Just $ OSResult (repF f) (Just v1) (Just v2) Nothing   Nothing
          mkRes [_,f,v1]          = Just $ OSResult (repF f) (Just v1) Nothing   Nothing   Nothing
          mkRes [_,f]             = Just $ OSResult (repF f) Nothing   Nothing   Nothing   Nothing
          mkRes _                 = Nothing

          repF x = maybe x id osFamRep


-------------------------------------------------------------------------------
-- | Result type for 'parseOS'
data OSResult = OSResult {
      osrFamily :: Text
    , osrV1     :: Maybe Text
    , osrV2     :: Maybe Text
    , osrV3     :: Maybe Text
    , osrV4     :: Maybe Text
    } deriving (Show,Read,Eq,Typeable,Data)

instance Default OSResult where
    def = OSResult "" Nothing Nothing Nothing Nothing


-------------------------------------------------------------------------------
-- | Construct a version string from 'OSResult'
osrVersion :: OSResult -> Text
osrVersion OSResult{..} =
    T.intercalate "." . catMaybes . takeWhile isJust $ [osrV1, osrV2, osrV3, osrV4]

                              -------------------
                              -- Parser Config --
                              -------------------

-------------------------------------------------------------------------------
-- | User-Agent string parser data
data UAConfig = UAConfig {
      uaParsers  :: [UAParser]
    , osParsers  :: [OSParser]
    , devParsers :: [DevParser]
    } deriving (Eq,Show)


-------------------------------------------------------------------------------
data UAParser = UAParser {
      uaRegex  :: Regex
    , uaFamRep :: Maybe Text
    , uaV1Rep  :: Maybe Text
    } deriving (Eq,Show)


-------------------------------------------------------------------------------
data OSParser = OSParser {
      osRegex  :: Regex
    , osFamRep :: Maybe Text
    , osRep1   :: Maybe Text
    , osRep2   :: Maybe Text
    } deriving (Eq,Show)


data DevParser = DevParser {
      devRegex :: Regex
    , devRep   :: Maybe Text
    } deriving (Eq,Show)


-------------------------------------------------------------------------------
parseRegex :: Object -> Parser Regex
parseRegex v = flip compile [] `liftM` (T.encodeUtf8 <$> v .: "regex")


-------------------------------------------------------------------------------
instance FromJSON UAConfig where
    parseJSON (Object v) =
      UAConfig
        <$> v .: "user_agent_parsers"
        <*> v .: "os_parsers"
        <*> v .: "device_parsers"
    parseJSON _ = error "Object expected when parsing JSON"


-------------------------------------------------------------------------------
instance FromJSON UAParser where
    parseJSON (Object v) =
      UAParser <$> parseRegex v
               <*> (v .:? "family_replacement" <|> return Nothing)
               <*> (v .:? "v1_replacement" <|> return Nothing)
    parseJSON _ = error "Object expected when parsing JSON"


-------------------------------------------------------------------------------
instance FromJSON OSParser where
    parseJSON (Object v) =
      OSParser <$> parseRegex v
               <*> (v .:? "os_replacement" <|> return Nothing)
               <*> (v .:? "os_v1_replacement" <|> return Nothing)
               <*> (v .:? "os_v2_replacement" <|> return Nothing)
    parseJSON _ = error "Object expected when parsing JSON"


-------------------------------------------------------------------------------
instance FromJSON DevParser where
    parseJSON (Object v) =
      DevParser <$> parseRegex v
                <*> v .:? "device_replacement"
    parseJSON _ = error "Object expected when parsing JSON"
