module IndicatorEngine ( calculateSMA, calculateEMA, calculateRSI, logInfo) where

import Data.Either (fromRight)
import System.CPUTime ( getCPUTime )
import Control.Monad (void)
import System.Random (randomRs, newStdGen)
import Data.List (foldl', genericLength)


-- Implementations of calculateSMA and calculateEMA)
-- Function to calculate SMA
calculateSMA :: [Double] -> Int -> Either String Double
calculateSMA prices period
    | period <= 0 = Left "Period must be positive"
    | length prices < period = Left "Not enough data to calculate SMA"
    | otherwise = Right $ sum (take period (reverse prices)) / fromIntegral period

-- Function to calculate EMA
calculateEMA :: [Double] -> Int -> Either String Double
calculateEMA prices period
    | period <= 0 = Left "Period must be positive"
    | length prices < period = Left "Not enough data to calculate EMA"
    | otherwise = Right $ emaCalc (take period prices) period
  where
    multiplier = 2 / fromIntegral (period + 1)
    emaCalc (x:xs) n = foldl' (\prevEma current -> (current - prevEma) * multiplier + prevEma) x xs

calculateRSI :: [Double] -> Int -> Either String Double
calculateRSI prices period
    | period <= 0 = Left "Period must be positive"
    | length prices < period + 1 = Left "Not enough data to calculate RSI"
    | otherwise = Right $ let
        changes = zipWith (-) (tail prices) prices
        (initialGains, initialLosses) = foldl' (\(gains, losses) change ->
            (max 0 change + gains, max 0 (-change) + losses)
            ) (0, 0) (take period changes)
        initialAvgGain = initialGains / fromIntegral period
        initialAvgLoss = initialLosses / fromIntegral period
        (finalAvgGain, finalAvgLoss) = foldl' (\(avgGain, avgLoss) change -> 
            let
                gain = max 0 change
                loss = max 0 (-change)
                newAvgGain = (avgGain * (fromIntegral period - 1) + gain) / fromIntegral period
                newAvgLoss = (avgLoss * (fromIntegral period - 1) + loss) / fromIntegral period
            in (newAvgGain, newAvgLoss)
            ) (initialAvgGain, initialAvgLoss) (drop period changes)
        finalRs = finalAvgGain / finalAvgLoss
        in 100 - (100 / (1 + finalRs))

-- Helper function to log information or errors (Dummy implementation)
logInfo :: String -> IO ()
logInfo msg = putStrLn ("Info: " ++ msg)

logError :: String -> IO ()
logError msg = putStrLn ("Error: " ++ msg)

