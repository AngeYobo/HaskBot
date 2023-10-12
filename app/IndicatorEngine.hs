module IndicatorEngine
    ( MovingAverage(..)
    , RSI(..)
    , IndicatorConfig(..)
    , calculateSMA
    , calculateEMA
    , calculateRSI
    , calculateIndicators
    , generateBuySignal
    , generateSellSignal
    ) where

import Data.List (take)
import Data.Maybe (fromMaybe)
import DataFetcher (CandlestickData(..))

data MovingAverage = SimpleMA | ExponentialMA
data RSI = RSI

data IndicatorConfig = IndicatorConfig
    { movingAverageType :: MovingAverage
    , movingAveragePeriod :: Int
    , rsiPeriod :: Int
    }

calculateSMA :: [Double] -> Int -> Maybe Double
calculateSMA prices period
    | length prices < period = Nothing
    | otherwise = Just $ sum (take period prices) / fromIntegral period

calculateEMA :: [Double] -> Int -> Maybe Double
calculateEMA prices period
    | length prices < period = Nothing
    | otherwise = Just ema
  where
    multiplier = 2 / fromIntegral (period + 1)
    ema = foldl (\prevEma x -> (x - prevEma) * multiplier + prevEma) (head prices) (drop 1 prices)

calculateRSI :: [Double] -> Int -> Maybe Double
calculateRSI prices period
    | length prices < period + 1 = Nothing
    | otherwise = Just rsi
  where
    changes = zipWith (-) (tail prices) prices
    gains = [x | x <- changes, x >= 0]
    losses = [-x | x <- changes, x < 0]
    avgGain = sum (take period gains) / fromIntegral period
    avgLoss = sum (take period losses) / fromIntegral period
    rs = if avgLoss == 0 then 0 else avgGain / avgLoss
    rsi = 100 - (100 / (1 + rs))

calculateIndicators :: [Double] -> IndicatorConfig -> [(String, Maybe Double)]
calculateIndicators prices config =
    [ ("SMA", calculateSMA prices (movingAveragePeriod config))
    , ("EMA", calculateEMA prices (movingAveragePeriod config))
    , ("RSI", calculateRSI prices (rsiPeriod config))
    ]

generateBuySignal :: Double -> Double -> Bool
generateBuySignal rsiValue threshold = rsiValue < threshold

generateSellSignal :: Double -> Double -> Bool
generateSellSignal rsiValue threshold = rsiValue > threshold
