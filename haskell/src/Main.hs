
module Main where

import BasePrelude

import PortReb.Types

sample_assets :: [RawAsset]
sample_assets = [ RawAsset "GOOG" "0.60" "52"  "98.0"
                , RawAsset "AAPL" "0.30" "136" "22.0"
                , RawAsset "TSLA" "0.10" "239" "8.0"
                ]

samplePort :: RawPortfolio
samplePort = RawPortfolio sample_assets "10000"

