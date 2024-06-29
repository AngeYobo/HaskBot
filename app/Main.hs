{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import DataFetcher (app, connectionAttempts, resetConnectionAttempts, checkConnectionLimit, attemptConnection, waitForConnectionLimitReset, bufferSize)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch, SomeException)
import Control.Monad (forever, void)
import System.Exit (exitSuccess)
import Data.IORef (newIORef, IORef, readIORef)
import DataProcessor (Kline(..), RingBuffer, initRingBuffer, getAllElements)
import IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)
import DisplayManager (displayMarketData, displayTradingSignals, displayTechnicalIndicators, displayOverallDashboard)

-- Main Logic Function
mainLogicMain :: IORef Int -> IORef (RingBuffer Kline) -> IO ()
mainLogicMain attemptsRef klineBufferRef = do
    allowed <- checkConnectionLimit attemptsRef
    if allowed
        then do
            attemptConnection attemptsRef klineBufferRef
            -- Regularly update and display dashboard
            void $ forkIO $ updateAndDisplayDashboard klineBufferRef
        else
            waitForConnectionLimitReset attemptsRef

updateAndDisplayDashboard :: IORef (RingBuffer Kline) -> IO ()
updateAndDisplayDashboard klineBufferRef = do
    forever $ do
        putStrLn "Attempting to update and display dashboard..."
        klineBuffer <- readIORef klineBufferRef
        let klines = getAllElements klineBuffer
        putStrLn $ "Number of Klines in buffer: " ++ show (length klines)
        displayOverallDashboard klineBufferRef
        threadDelay 10000000       

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

displayDashboard :: Maybe Double -> Maybe Double -> Maybe Double -> IO ()
displayDashboard sma ema rsi = do
    putStrLn "----- Dashboard -----"
    -- Display indicators and signals
    putStrLn "---------------------"

main :: IO ()
main = do
    putStrLn "Welcome to HaskBot! Initializing..."    
    -- Initialize shared resources
    attemptsRef <- connectionAttempts
    klineBufferRef <- newIORef (initRingBuffer bufferSize)
    -- Start the main application logic (receiving and processing data)
    forkIO $ catch (mainLogicMain attemptsRef klineBufferRef) handleException    
    -- Continuously update and display the dashboard
    forever $ do
        displayOverallDashboard klineBufferRef
        threadDelay 1000000  -- 1 second delay
    putStrLn "Exiting HaskBot..."
-- Exception handling function
handleException :: SomeException -> IO ()
handleException e = do
    putStrLn $ "An error occurred: " ++ show e
    -- Handle the exception or exit
    exitSuccess  -- Exit the program
