
module PortReb.Rebalancer where

import BasePrelude

import PortReb.Types

type TrackingBand = Rational

defaultTrackingBand :: TrackingBand
defautlTrackingBand = 5 % 100

rebalance :: ValidPortfolio -> OutputPortfolio
rebalance = undefined

rebAsset :: TrackingBand -> PortfolioSize -> ValidAsset -> [RebalancedAsset]
rebAsset band portSize' (ValidAsset sym' alloc' qty' price') =
  rebAsset' `fmap` [newQty | newQty <- [lbound..ubound]]
  where
    lbound :: Integer
    lbound = ceiling $ (((-1 * band) + 1) * alloc' * portSize') / price'
    ubound :: Integer
    ubound = floor $ ((band + 1) * alloc' * portSize') / price'
    rebAsset' = \newQty -> RebalancedAsset sym' ((newQty * price') % portSize') (newQty) price'
