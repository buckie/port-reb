
module Main where

import BasePrelude

type PortfolioSize = Int

data Asset = Asset
      { symbol :: String
      , alloc :: Float
      } deriving (Eq, Show, Read)

data TargetPortfolio = TargetPortfolio
      { assests :: [Asset]
      , portSize :: PortfolioSize
      } deriving (Show)

data CurrentPortfolio = CurrentPortfolio [Asset]
      deriving (Show)





