
module PortReb.Rebalancer where

import BasePrelude

import PortReb.Types

rebAsset :: TrackingBand -> PortfolioSize -> ValidAsset -> [RebalancedAsset]
rebAsset band portSize' (ValidAsset sym' alloc' _ price') =
  rebAsset' `fmap` [newQty | newQty <- [lbound..ubound]]
  where
    lbound :: Integer
    lbound = ceiling $ (((-1 * band) + 1) * alloc' * portSize') / price'
    ubound :: Integer
    ubound = floor $ ((band + 1) * alloc' * portSize') / price'
    newAlloc :: Integer -> Rational
    newAlloc q = ((toRational q) * price') / portSize'
    rebAsset' :: Integer -> RebalancedAsset
    rebAsset' = \newQty -> ValidAsset sym' (newAlloc newQty) (newQty) price'

rebalance :: TrackingBand -> ValidPortfolio -> RebalancedPortfolio
rebalance band (ValidPortfolio assets size) = ValidPortfolio bestAssetReb (notional bestAssetReb)
  where
    allValidRebs :: [[RebalancedAsset]]
    allValidRebs = filter checkPort $ sequence $ fmap (rebAsset band size) assets
    bestAssetReb :: [RebalancedAsset]
    bestAssetReb = maximumBy (comparing notional) $ allValidRebs

-- | Check if a portfolio is valid by summing the allocations. Iff the total allocation is > 1 then it is invalid
checkPort :: [RebalancedAsset] -> Bool
checkPort assets' = 1 >= (sum $ alloc `fmap` assets')

notional :: [RebalancedAsset] -> Rational
notional assets' = sum $ (\x -> (price x) * (toRational $ qty x)) `fmap` assets'
