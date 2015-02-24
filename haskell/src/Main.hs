
module Main where

import BasePrelude
import Data.Aeson
import Data.Text

data Asset = Asset
      { symbol :: !Text
      , alloc  :: !Float
      , qty    :: !Int
      , price  :: !Float
      } deriving (Eq, Show, Read, Generic)

instance FromJSON Asset
instance ToJSON Asset

type PortfolioSize = Float

data Portfolio = Portfolio
      { assests  :: ![Asset]
      , portSize :: !PortfolioSize
      } deriving (Show, Generic)

instance FromJSON Portfolio
instance ToJSON Portfolio

type CurrentPortfolio = Portfolio
type InputPortfolio = Portfolio

samplePort :: Portfolio
samplePort = Portfolio [ Asset "GOOG" 0.60 52  98.0
                       , Asset "AAPL" 0.30 136 22.0
                       , Asset "TSLA" 0.10 239 8.0
                       ]
                       10000

validSym a =
