{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Applicative     as A
import           Control.DeepSeq
import           Criterion.Main
import           Data.DeriveTH
-------------------------------------------------------------------------------
import           Web.UAParser
import           Web.UAParser.SuiteUtils
-------------------------------------------------------------------------------


main :: IO ()
main = do
  uas <- take 100 A.<$> loadTests "test_resources/firefox_user_agent_strings.yaml"
  oses <- take 100 <$> loadTests "test_resources/additional_os_tests.yaml"
  devs <- take 100 <$> loadTests "tests/test_device.yaml"
  defaultMain [ bench "Parsing 100 UAs" $ nf (map (parseUA . uatcString)) uas
              , bench "Parsing 100 OSes" $ nf (map (parseOS . ostcString)) oses
              , bench "Parsing 100 Devices" $ nf (map (parseDev . dtcString)) devs
              ]


$(derives [makeNFData] [''UAResult, ''OSResult, ''DevResult])
