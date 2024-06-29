{-# LANGUAGE ScopedTypeVariables #-}

module DataFetcher (app, connectionAttempts, mainLogic, resetConnectionAttempts, checkConnectionLimit, attemptConnection, waitForConnectionLimitReset, bufferSize) where
   
import           Control.Concurrent (killThread, forkIO, threadDelay, newMVar, modifyMVar_, readMVar)
import           Control.Exception (SomeException, try, finally, handle, SomeException, catch, IOException, try, bracket, bracketOnError, Exception, throwIO, fromException, AsyncException(..))
import           Control.Monad (forever, void, when, unless, forM_ )
import           Data.Aeson (encode, object, (.=), ToJSON(..), FromJSON(..), Value(..), withObject, (.:), (.:?), (.!=), (.:!), eitherDecode)
import           Data.Aeson.Key (fromString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as B
import           Data.IORef (IORef, atomicWriteIORef, modifyIORef', newIORef, readIORef, writeIORef)
import           DataProcessor (printClosePrices, printRingBufferContents, WebSocketMessage (..), GeneralMessage(..), KlineMessage(..), Kline(..), RingBuffer(..), initRingBuffer, processKlineData, getAllElements, queryRingBuffer, readDouble, extractClosingPrices)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)
import           Network.WebSockets (ClientApp, receiveData, sendTextData, defaultConnectionOptions, runClient, Connection)
import           SignalGenerator (generateSignals, Signal(..), BuySignal(..), SellSignal(..), Thresholds(..), PriceData(..), MovingAverage(..), SignalHistory(..), recordSignal, isSMABullish, isRSIOversold, isSMABearish, isRSIOverbought, isGoldenCross, isDeathCross)   
import           System.Exit (exitSuccess)
import           Wuss (runSecureClient)

-- Constants
bufferSize :: Int          
bufferSize = 16 -- can ajust buffer here

-- The endpoint for the Bybit WebSocket API
host :: String
host = "stream.bybit.com"

-- Path for the Perpetual USDT public Kline data stream
path :: String
path = "/contract/usdt/public/v3"

-- The WebSocket client application, now using bracket to ensure cleanup
app :: Connection -> IORef (RingBuffer Kline) -> IO ()
app conn klineBufferRef = do
    putStrLn "Connected!"
    bracket
        (forkIO $ heartbeat conn)
        (\tid -> do
            putStrLn "Cleaning up heartbeat thread"
            killThread tid)
        (\_ -> do
            let subscribeMessage = encode $ object
                    [ fromString "op" .= ("subscribe" :: String)
                    , fromString "args" .= (["kline.1.ADAUSDT"] :: [String])
                    ]
            sendTextData conn subscribeMessage
            void . forever $ do
                message <- receiveData conn
                case eitherDecode message :: Either String WebSocketMessage of
                    Right (KlineMsg klineMsg) -> do
                        putStrLn $ "\x1b[32mKline data received:\x1b[0m" ++ show klineMsg 
                        -- Process and add Kline data to the RingBuffer
                        forM_ (klineDataField klineMsg) $ \kline -> do
                            modifyIORef' klineBufferRef (`processKlineData` kline)
                    Right (GeneralMsg generalMsg) -> do
                        putStrLn "PONG received."
                    Left error -> do
                        putStrLn $ "Error parsing message: " ++ error                
            )
    putStrLn "Disconnected."
    sendClose conn (LBS.pack "Bye!")

-- Constants
second :: Int
second = 1000000  -- Number of microseconds in one second

-- The heartbeat function
heartbeat :: Connection -> IO ()
heartbeat conn = forever $ do
    sendTextData conn (encode $ object [fromString "op" .= "ping"])
    -- print ping in green
    putStrLn "\x1b[32mSent ping\x1b[0m"
    threadDelay $ 20 * second -- 20 seconds
    `catch` \e -> do
        let err = show (e :: SomeException)
        when (err == show UserInterrupt)
  $ do putStrLn "Interrupted by user. Exiting..."
       throwIO e -- Other exceptions are not re-thrown, effectively ignored

-- Send a close request to the server
sendClose :: Connection -> LBS.ByteString -> IO ()
sendClose = sendTextData

-- Function to handle reconnection with exponential backoff
reconnectWithBackoff :: Int -> IORef (RingBuffer Kline) -> IO ()
reconnectWithBackoff attempt klineBufferRef = do
    let delay = exponentialBackoff attempt
    putStrLn $ "Waiting " ++ show delay ++ " seconds before reconnecting..."
    threadDelay (delay * second)  -- Use `second` here
    when (delay < maxDelay) $ do
        putStrLn "Attempting to reconnect..."
        result <- try $ runSecureClient host 443 path (`app` klineBufferRef)
        case result of
            Right _ -> putStrLn "Connection was closed gracefully."
            Left e  -> do
                putStrLn $ "Connection error: " ++ show (e :: SomeException)
                reconnectWithBackoff (attempt + 1) klineBufferRef

-- Constants for reconnection strategy
baseDelay :: Int
baseDelay = 5 -- Base delay in seconds

maxDelay :: Int
maxDelay = 60 -- Maximum delay in seconds

exponentialBackoff :: Int -> Int
exponentialBackoff attempt = min maxDelay (baseDelay * 2 ^ attempt)

-- We use IORef to safely modify shared state in an IO context
connectionAttempts :: IO (IORef Int)
connectionAttempts = newIORef 0

checkConnectionLimit :: IORef Int -> IO Bool
checkConnectionLimit attemptsRef = do
    attempts <- readIORef attemptsRef
    return (attempts < 500)

incrementConnectionAttempts :: IORef Int -> IO ()
incrementConnectionAttempts attemptsRef = modifyIORef' attemptsRef (+1)

-- The resetConnectionAttempts loop
resetConnectionAttempts :: IORef Int -> IO ()
resetConnectionAttempts attemptsRef = loop where
  loop = do
    threadDelay $ 5 * 60 * second -- Wait for 5 minutes, use `second` here
    atomicWriteIORef attemptsRef 0
    loop  -- Recursive call to loop itself after the delay

-- Function to wait until the connection limit is reset
waitForConnectionLimitReset :: IORef Int -> IO ()
waitForConnectionLimitReset attemptsRef = do
    putStrLn "Connection limit reached, waiting for reset..."
    waitLoop
  where
    waitLoop = do
      threadDelay $ 60 * second -- Wait for 1 minute before checking again
      allowed <- checkConnectionLimit attemptsRef
      unless allowed waitLoop

-- Function to attempt a connection
attemptConnection :: IORef Int -> IORef (RingBuffer Kline) -> IO ()
attemptConnection attemptsRef klineBufferRef = do
    currentAttempt <- readIORef attemptsRef
    result <- try $ runSecureClient host 443 path (`app` klineBufferRef)
    case result of
        Right _ -> do
            putStrLn "Connection was closed gracefully."
            writeIORef attemptsRef 0  -- Reset attempts after a successful connection
        Left e  -> do
            putStrLn $ "Connection error: " ++ show (e :: SomeException)
            incrementConnectionAttempts attemptsRef  -- Increment only after a failed attempt
            reconnectWithBackoff currentAttempt klineBufferRef

-- Function to get the latest market data
getLatestMarketData :: IORef (RingBuffer Kline) -> IO Kline
getLatestMarketData klineBufferRef = do
    klineBuffer <- readIORef klineBufferRef
    let latestKline = last (getAllElements klineBuffer)
    return latestKline

mainLogic :: IORef Int -> IO ()
mainLogic attemptsRef = do
    allowed <- checkConnectionLimit attemptsRef
    if allowed
      then do
        klineBufferRef <- newIORef (initRingBuffer bufferSize :: RingBuffer Kline)
        attemptConnection attemptsRef klineBufferRef
      else waitForConnectionLimitReset attemptsRef

-- Main entry point
main :: IO ()
main = do
    attemptsRef <- connectionAttempts -- Create the shared state
    forkIO $ resetConnectionAttempts attemptsRef -- Start the reset timer
    catch (mainLogic attemptsRef) $ \(e :: SomeException) -> do
        print e
        putStrLn "An unexpected error occurred."
    exitSuccess

