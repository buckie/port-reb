
module PortReb.Rebalancer where

import BasePrelude

import PortReb.Types

type TrackingBand = Rational

trackingBand :: TrackingBand
trackingBand = 5 % 100

rebalance :: ValidPortfolio -> OutputPortfolio
rebalance = undefined

rebAsset :: ValidAsset -> [RebalancedAsset]
rebAsset PortfolioSize (ValidAsset sym' alloc' qty' price') = [ValidAsset sym' alloc' (newQty x) price' | x <-
  where

