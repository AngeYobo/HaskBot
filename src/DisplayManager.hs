module DisplayManager where

import DataProcessor (Kline(..), RingBuffer, getAllElements, klineBufferRef, initRingBuffer, bufferSize, processKlineData, RingBuffer(..), addRingBuffer, processKlineData )
import IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)
import SignalGenerator (generateSignals, Signal)
import Data.IORef (IORef, readIORef, modifyIORef', newIORef)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime, UTCTime(..), secondsToDiffTime) 
import Data.Time.Calendar (fromGregorian)
import Data.Text (Text, pack)

-- Custom display function for Kline
displayKline :: Kline -> String
displayKline kline =
    "Open Time: " ++ show (start kline) ++
    ", Open: " ++ show (open kline) ++
    ", High: " ++ show (high kline) ++
    ", Low: " ++ show (low kline) ++
    ", Close: " ++ show (close kline) ++
    ", Volume: " ++ show (volume kline)

-- Display current market data
displayMarketData :: [Kline] -> IO ()
displayMarketData klines = do
    putStrLn "------ Market Data ------------------------------------------------------------------------------------------"
    mapM_ (putStrLn . displayKline) klines
    putStrLn "---------------------------------------------------------------------------------------------------------------"

-- Custom display function for Signal
displaySignal :: Signal -> String
displaySignal signal = "Signal: " ++ show signal

-- Display trading signals
displayTradingSignals :: [Kline] -> IO ()
displayTradingSignals klines = do
    signals <- generateSignals klines
    putStrLn "------ Trading Signals ----------------------------------------------------------------------------------------"
    mapM_ (putStrLn . displaySignal) signals    

-- Display SMA
displaySMA :: [Double] -> IO ()
displaySMA prices = putStrLn $ "SMA: " ++ either id show (calculateSMA prices 14)

-- Display EMA
displayEMA :: [Double] -> IO ()
displayEMA prices = putStrLn $ "EMA: " ++ either id show (calculateEMA prices 14)

-- Display RSI
displayRSI :: [Double] -> IO ()
displayRSI prices = putStrLn $ "RSI: " ++ either id show (calculateRSI prices 14)

-- Combined function to display all technical indicators
displayTechnicalIndicators :: [Double] -> IO ()
displayTechnicalIndicators prices = do
    putStrLn "------ Technical Indicators -------------------------------------------------------------------------------------"
    displaySMA prices
    displayEMA prices
    displayRSI prices
    --putStrLn "-----------------------------------------------------------------------------------------------------------------"

-- Display overall dashboard
displayOverallDashboard :: IORef (RingBuffer Kline) -> IO ()
displayOverallDashboard klineBufferRef = do
    klineBuffer <- readIORef klineBufferRef
    let klines = getAllElements klineBuffer
    putStrLn $ "Debug: Number of Klines in RingBuffer: " ++ show (length klines)
    displayMarketData klines
    displayTradingSignals klines
    displayTechnicalIndicators (map close klines)
    putStrLn "-------------------------------------------------------------------------------------------------------------------"

-- Function to get the latest market data
getLatestMarketData :: IORef (RingBuffer Kline) -> IO (Maybe Kline)
getLatestMarketData klineBufferRef = do
    klineBuffer <- readIORef klineBufferRef
    return $ if null (getAllElements klineBuffer)
             then Nothing
             else Just (last (getAllElements klineBuffer))


