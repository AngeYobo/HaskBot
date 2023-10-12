-- app/Main.hs

module Main where

import DataFetcher 
    ( fetchCandlestickData
    , formatCandlestickData
    , CandlestickData(..)
    , closePrice
    )

import IndicatorEngine
    ( MovingAverage(..)
    , RSI(..)
    , IndicatorConfig(..)
    , calculateSMA
    , calculateEMA
    , calculateRSI
    , calculateIndicators
    )

import SignalGenerator 
    ( Thresholds(..)
    , generateBuySignals
    , generateSellSignals
    )

import UserInterface ()

main :: IO ()
main = do
    putStrLn "Fetching candlestick data..."
    candlestickData <- fetchCandlestickData
    putStrLn "Received candlestick data:"

    let formattedData = map formatCandlestickData candlestickData
    mapM_ putStrLn formattedData  -- Print the formatted data

    let prices = map closePrice candlestickData
        config = IndicatorConfig SimpleMA 3 3  -- Example configuration, adjust as needed
        indicatorResults = calculateIndicators prices config

    let smaValue = case lookup "SMA" indicatorResults of
                    Just (Just sma) -> sma
                    _ -> error "SMA not found in indicator results"

    let emaValue = case lookup "EMA" indicatorResults of
                    Just (Just ema) -> ema
                    _ -> error "EMA not found in indicator results"

    let rsiValue = case lookup "RSI" indicatorResults of
                    Just (Just rsi) -> rsi
                    _ -> error "RSI not found in indicator results"

    -- Print indicators
    putStrLn $ "SMA: " ++ show smaValue
    putStrLn $ "EMA: " ++ show emaValue
    putStrLn $ "RSI: " ++ show rsiValue

    let smaThresholds = Thresholds { buyThreshold = 0, sellThreshold = 0, holdThreshold = 0, strongBuyThreshold = 0 }
    let emaThresholds = Thresholds { buyThreshold = 0, sellThreshold = 0, holdThreshold = 0, strongBuyThreshold = 0 }
    let rsiThresholds = Thresholds { buyThreshold = 30, sellThreshold = 70, holdThreshold = 0, strongBuyThreshold = 0 }

    let smaBuySignal = generateBuySignals [smaValue] (buyThreshold smaThresholds)
    let smaSellSignal = generateSellSignals [smaValue] (sellThreshold smaThresholds)

    let emaBuySignal = generateBuySignals [emaValue] (buyThreshold emaThresholds)
    let emaSellSignal = generateSellSignals [emaValue] (sellThreshold emaThresholds)

    let rsiBuySignal = generateBuySignals [rsiValue] (buyThreshold rsiThresholds)
    let rsiSellSignal = generateSellSignals [rsiValue] (sellThreshold rsiThresholds)
    
    -- Print signals for each indicator
    putStrLn "SMA Buy Signal:"
    print smaBuySignal
    putStrLn "SMA Sell Signal:"
    print smaSellSignal
    putStrLn "EMA Buy Signal:"
    print emaBuySignal
    putStrLn "EMA Sell Signal:"
    print emaSellSignal
    putStrLn "RSI Buy Signal:"
    print rsiBuySignal
    putStrLn "RSI Sell Signal:"
    print rsiSellSignal
