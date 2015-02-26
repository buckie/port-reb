
module Main where

import BasePrelude

import PortReb.Types

import Data.Validation

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

