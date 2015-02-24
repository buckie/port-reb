
module Main where

import BasePrelude
import Numeric

import Control.Lens

import Data.Aeson
import Data.Validation

type Symbol = String

data RawAsset = RawAsset
      { raw_symbol :: !String
      , raw_alloc  :: !String
      , raw_qty    :: !String
      , raw_price  :: !String
      } deriving (Show, Generic)

instance FromJSON RawAsset

data ValidAsset = ValidAsset
      { symbol :: !Symbol
      , alloc  :: !Rational
      , qty    :: !Integer
      , price  :: !Rational
      } deriving (Show, Generic)

instance ToJSON ValidAsset

mkSymbol :: String -> AccValidation [Error] Symbol
mkSymbol s = case length s of
                  0 -> _Failure # [NullSymbol]
                  _ -> _Success # s

mkRational :: String -> Either Error Rational
mkRational s = case readFloat s of
                   [(num, [])] -> Right num
                   [(_, leftovers)] -> Left $ InvalidRational s leftovers
                   _ -> error "You aren't be able to hit this point..."

mkAlloc :: String -> AccValidation [Error] Rational
mkAlloc s = case mkRational s of
                 Left err -> _Failure # [err]
                 Right num -> if num >= 0 && num <= 1
                                 then _Success # num
                                 else _Failure # [InvalidAlloc num]

mkQty :: String -> AccValidation [Error] Integer
mkQty s = case readMaybe s of
               Nothing -> _Failure # [InvalidQty s]
               Just num -> if num >= 0
                              then _Success # num
                              else _Failure # [InvalidQty s]

mkPrice :: String -> AccValidation [Error] Rational
mkPrice s = case mkRational s of
                 Left err -> _Failure # [err]
                 Right num -> if num > 0
                                 then _Success # num
                                 else _Failure # [InvalidPrice num]

mkValidAsset :: RawAsset -> AccValidation Error ValidAsset
mkValidAsset (RawAsset symbol' alloc' qty' price') =
  case asset of
       AccFailure err -> _Failure # AssetErrors symbol' err
       AccSuccess v -> _Success # v
  where
    asset = ValidAsset <$> mkSymbol symbol'
                       <*> mkAlloc alloc'
                       <*> mkQty qty'
                       <*> mkPrice price'

data RawPortfolio = RawPortfolio
      { raw_assests  :: ![RawAsset]
      , raw_portSize :: !String
      } deriving (Show, Generic)

instance FromJSON RawPortfolio

validateAssets :: [RawAsset] -> AccValidation [Error] ValidAssetCollection
validateAssets rawAssets =
  case failedAssets of
       [] -> case (totalAlloc validAssets) of
                              1 -> _Success # validAssets
                              t -> _Failure # [TotalAllocSize t]
       _ -> _Failure # failedAssets
  where
    assets' = mkValidAsset `fmap` rawAssets
    failedAssets = collectErrors assets'
    validAssets = collectValid assets'

totalAlloc :: ValidAssetCollection -> Rational
totalAlloc v = sum $ alloc `fmap` v

collectErrors :: [AccValidation Error ValidAsset] -> [Error]
collectErrors ((AccFailure err): xs) = err : collectErrors xs
collectErrors (_ : xs) = collectErrors xs
collectErrors [] = []

collectValid :: [AccValidation Error ValidAsset] -> [ValidAsset]
collectValid ((AccSuccess validAsset): xs) = validAsset : collectValid xs
collectValid (_ : xs) = collectValid xs
collectValid [] = []

type PortfolioSize = Rational

mkValidPortSize :: String -> AccValidation [Error] PortfolioSize
mkValidPortSize s = case mkRational s of
                         Left err -> _Failure # [err]
                         Right num -> if num > 0
                                         then _Success # num
                                         else _Failure # [NonPositivePortfolioSize num]

type ValidAssetCollection = [ValidAsset]

mkValidPortfolio :: RawPortfolio -> AccValidation Error ValidPortfolio
mkValidPortfolio rawPort@(RawPortfolio assets size) =
  case port' of
       AccSuccess validPort -> _Success # validPort
       AccFailure errs -> _Failure # InvalidPortfolio rawPort errs
  where
    port' = ValidPortfolio
              <$> validateAssets assets
              <*> mkValidPortSize size

data ValidPortfolio = ValidPortfolio
      { assests  :: ![ValidAsset]
      , portSize :: !Rational
      } deriving (Show, Generic)

instance ToJSON ValidPortfolio

type CurrentPortfolio = ValidPortfolio
type InputPortfolio = ValidPortfolio

data Error = NullSymbol
           | InvalidRational String String
           | InvalidAlloc Rational
           | InvalidQty String
           | InvalidPrice Rational
           | NonPositivePortfolioSize PortfolioSize
           | TotalAllocSize Rational
           | AssetErrors String [Error]
           | InvalidPortfolio RawPortfolio [Error]

instance Show Error where
  show = showError

showError :: Error -> String
showError NullSymbol = "The symbol for this asset was absent"
showError (InvalidRational s leftovers) = "Unable to parse \"" ++ show s ++ "\" into rational due to \"" ++ show leftovers ++ "\""
showError (InvalidQty v) = "Quantities must be an integer greater than zero: " ++ show v
showError (InvalidPrice v) = "Prices must be greater than 0: " ++ show v
showError (InvalidAlloc v) = "Allocations are bounded by [0,100]: " ++ show v
showError (NonPositivePortfolioSize v) = "The Portfolio Size must be greater than 0 but was given as: " ++ show v
showError (TotalAllocSize v) = "The portfolio's total allocation must sum to 100: " ++ show v
showError (AssetErrors v errs) = "The asset \"" ++ show v ++ "\" has the following errors: " ++ show errs
showError (InvalidPortfolio v errs) = "The asset \"" ++ show v ++ "\" has the following errors: " ++ show errs

sample_assets :: [RawAsset]
sample_assets = [ RawAsset "GOOG" "0.60" "52"  "98.0"
                , RawAsset "AAPL" "0.30" "136" "22.0"
                , RawAsset "TSLA" "0.10" "239" "8.0"
                ]

samplePort :: RawPortfolio
samplePort = RawPortfolio sample_assets "10000"

