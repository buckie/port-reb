
module Main where

import BasePrelude

import Data.Validation
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

import PortReb.Types
import PortReb.Rebalancer

sAssets :: [RawAsset]
sAssets = [ RawAsset "GOOG" "0.60" "52"  "98.0"
          , RawAsset "AAPL" "0.30" "136" "22.0"
          , RawAsset "TSLA" "0.10" "239" "8.0"
          ]

sPort :: RawPortfolio
sPort = RawPortfolio sAssets "10000"

sValid :: ValidPortfolio
sValid = case mkValidPortfolio sPort of
              AccSuccess v -> v
              AccFailure v -> error $ show v


main :: IO ()
main = do
  putStrLn "Input Portfolio"
  B.putStrLn $ encode $ sValid
  putStrLn "Rebalanced Portfolio (tracking error = 5%)"
  B.putStrLn $ encode $ rebalance defautlTrackingBand sValid



