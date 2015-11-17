{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.DeepSeq
import           Criterion.Main
import           Data.DeriveTH
-------------------------------------------------------------------------------
import           Web.UAParser
import           Web.UAParser.SuiteUtils
-------------------------------------------------------------------------------


main :: IO ()
main = do
  cases <- loadTests "firefox_user_agent_strings.yaml"
  let ua = bench "UA Parsing" $ nf (map (parseUA . uatcString)) cases
  print $ show (length cases) ++ " strings being parsed."
  defaultMain [ua]


$(derives [makeNFData] [''UAResult])
