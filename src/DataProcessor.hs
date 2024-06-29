{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module DataProcessor (WebSocketMessage (..)
, KlineMessage(..), Kline(..), RingBuffer(..), GeneralMessage(..)
, addRingBuffer, initRingBuffer, processKlineData, getAllElements, queryRingBuffer
, readDouble, printDashboard, closingPriceAbove, queryClosingPriceAbove
, bufferSize, klineBufferRef, extractClosingPrices, getLatestMarketData
, printRingBufferContents, printClosePrices) 
where

import           Data.Aeson (FromJSON, parseJSON, withObject, (.:), eitherDecode, Value(..), (.:?), fromJSON)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Aeson.Key (fromString)
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM 
import           Control.Monad (forM_)
import           IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)    
import           Data.Maybe (catMaybes)
import           Data.IORef (newIORef, IORef, readIORef)

bufferSize :: Int
bufferSize = 16
-- Kline Data Structure
data Kline = Kline
    { start     :: UTCTime
    , end       :: UTCTime
    , interval  :: Text
    , open      :: Double
    , close     :: Double
    , high      :: Double
    , low       :: Double
    , volume    :: Double
    , turnover  :: Double
    , confirm   :: Bool
    , timestamp :: UTCTime
    } deriving (Show, Generic, Eq)

-- Define the FromJSON instance for Kline
instance FromJSON Kline where
    parseJSON = withObject "Kline" $ \v -> Kline
        <$> (posixSecondsToUTCTime . (/ 1000) <$> v .: fromString "start")
        <*> (posixSecondsToUTCTime . (/ 1000) <$> v .: fromString "end")
        <*> v .: fromString "interval"
        <*> (readDouble <$> v .: fromString "open")
        <*> (readDouble <$> v .: fromString "close")
        <*> (readDouble <$> v .: fromString "high")
        <*> (readDouble <$> v .: fromString "low")
        <*> (readDouble <$> v .: fromString "volume")
        <*> (readDouble <$> v .: fromString "turnover")
        <*> v .: fromString "confirm"
        <*> (posixSecondsToUTCTime . (/ 1000) <$> v .: fromString "timestamp")

-- Define the KlineMessage data structure
data KlineMessage = KlineMessage
    { klineTopic      :: Text
    , klineDataField  :: [Kline]
    , klineTs         :: Integer
    , klineMessageType:: Text
    } deriving (Show, Generic)

instance FromJSON KlineMessage where
    parseJSON :: Value -> Parser KlineMessage
    parseJSON = withObject "KlineMessage" $ \v -> KlineMessage
        <$> v .: "topic"
        <*> v .: "data"
        <*> v .: "ts"
        <*> v .: "type"

data GeneralMessage = GeneralMessage
    { gmSuccess :: Bool
    , gmRetMsg  :: Text
    , gmConnId  :: Text
    , gmReqId   :: Text
    , gmOp      :: Text
    } deriving (Show, Generic)

instance FromJSON GeneralMessage where
    parseJSON :: Value -> Parser GeneralMessage
    parseJSON = withObject "GeneralMessage" $ \v -> GeneralMessage
        <$> v .: "success"
        <*> v .: "ret_msg"
        <*> v .: "conn_id"
        <*> v .: "req_id"
        <*> v .: "op"

data WebSocketMessage
    = KlineMsg KlineMessage
    | GeneralMsg GeneralMessage 
    | OtherMsg
    deriving (Show, Generic)

instance FromJSON WebSocketMessage where
    parseJSON :: Value -> Parser WebSocketMessage
    parseJSON = withObject "WebSocketMessage" $ \v -> do
        op <- v .:? "op" :: Parser (Maybe Text)
        topic <- v .:? "topic" :: Parser (Maybe Text)
        case (op, topic) of
            (Just "ping", _) -> GeneralMsg <$> parseJSON (Object v)
            (Just "subscribe", _) -> GeneralMsg <$> parseJSON (Object v)
            (_, Just "kline.1.ADAUSDT") -> KlineMsg <$> parseJSON (Object v)
            _ -> return OtherMsg

-- Helper function to safely parse a string to Double
readDouble :: String -> Double
readDouble str = read str :: Double

-- Helper Functions
posixMillisecondsToUTCTime :: Integer -> UTCTime
posixMillisecondsToUTCTime = posixSecondsToUTCTime . (/ 1000) . fromIntegral

klineBufferRef :: RingBuffer Kline
klineBufferRef = initRingBuffer bufferSize :: RingBuffer Kline 

-- Function to print the contents of the Ring Buffer
printRingBufferContents :: RingBuffer Kline -> IO ()
printRingBufferContents rb = do
    let elements = getAllElements rb
    putStrLn "Current contents of the Ring Buffer:"
    mapM_ print elements
    putStrLn "End of Ring Buffer contents\n"

-- Function to parse Kline JSON data
parseKlineData :: B.ByteString -> Either String Kline
parseKlineData = eitherDecode

-- Define the Ring Buffer Data Structure
data RingBuffer a = RingBuffer
    { buffer :: V.Vector (Maybe a)     -- The vector to store data
    , index  :: Int            -- Current position in the buffer
    , maxSize :: Int           -- Maximum size of the buffer
    } deriving (Show)

-- Initialize an empty RingBuffer
initRingBuffer :: Int -> RingBuffer a
initRingBuffer size = RingBuffer
    { buffer = V.replicate size Nothing
    , index = 0
    , maxSize = size
    }
-- Implement Ring Buffer Operations
addRingBuffer :: RingBuffer a -> a -> RingBuffer a
addRingBuffer rb newElement = rb
    { buffer = V.unsafeUpd (buffer rb) [(currIndex, Just newElement)]
    , index = newIndex
    }
  where
    currIndex = index rb
    newIndex = (currIndex + 1) `mod` maxSize rb

-- Retrieve all elements from the RingBuffer in order
getAllElements :: RingBuffer a -> [a]
getAllElements rb = catMaybes . V.toList $ buffer rb

inspectRingBuffer :: RingBuffer Kline -> IO ()
inspectRingBuffer rb = do
    let elements = getAllElements rb
    putStrLn "Current contents of the Ring Buffer:"
    mapM_ print elements

extractClosingPrices :: RingBuffer Kline -> [Double]
extractClosingPrices buffer = map close $ getAllElements buffer

-- Function to print close prices from the ring buffer
printClosePrices :: IORef (RingBuffer Kline) -> IO ()
printClosePrices klineBufferRef = do
    klineBuffer <- readIORef klineBufferRef
    let klines = getAllElements klineBuffer
    let closePrices = map close klines
    putStrLn "Close Prices:"
    mapM_ print closePrices

-- Integrate Ring Buffer into DataProcessor
processKlineData :: RingBuffer Kline -> Kline -> RingBuffer Kline
processKlineData = addRingBuffer

-- When calculating an indicator
calculateIndicator :: RingBuffer Kline -> Double -- Placeholder return type
calculateIndicator rb = 
    let klines = getAllElements rb
    in 0.0 

-- Function to query elements based on a predicate
queryRingBuffer :: (a -> Bool) -> RingBuffer a -> [a]
queryRingBuffer predicate rb = filter predicate (getAllElements rb)

-- Example predicate: Kline closing price above a threshold
closingPriceAbove :: Double -> Kline -> Bool
closingPriceAbove threshold kline = close kline > threshold

-- Example usage: query all Klines with a closing price above a certain value
queryClosingPriceAbove :: Double -> RingBuffer Kline -> [Kline]
queryClosingPriceAbove threshold = queryRingBuffer (closingPriceAbove threshold)

-- Function to get the latest market data
getLatestMarketData :: IORef (RingBuffer Kline) -> IO Kline
getLatestMarketData klineBufferRef = do
    klineBuffer <- readIORef klineBufferRef
    let latestKline = last (getAllElements klineBuffer)
    return latestKline

-- Function to print Kline data and indicators
printDashboard :: RingBuffer Kline -> IO ()
printDashboard rb = do
    let klines = getAllElements rb
    let prices = map close klines

    putStrLn "----- Dashboard -----"
    putStrLn "Current Kline Data:"
    mapM_ print klines  -- Print Kline data

    -- Calculate and print indicators
    case calculateSMA prices 15 of
        Right sma -> putStrLn $ "SMA: " ++ show sma
        Left error -> putStrLn $ "SMA calculation error: " ++ error

    case calculateEMA prices 15 of
        Right ema -> putStrLn $ "EMA: " ++ show ema
        Left error -> putStrLn $ "EMA calculation error: " ++ error

    case calculateRSI prices 15 of
        Right rsi -> putStrLn $ "RSI: " ++ show rsi
        Left error -> putStrLn $ "RSI calculation error: " ++ error
    putStrLn "---------------------"   

connectToBuffer :: RingBuffer Kline -> IO ()
connectToBuffer rb = do
    let klines = getAllElements rb
    let prices = map close klines
    putStrLn "----- Debugging Info -----"
    print prices  -- Debug: Print extracted prices
    putStrLn "---------------------"
    -- Calculate and print indicators
    let sma = calculateIndicator rb
    putStrLn $ "SMA: " ++ show sma
    let ema = calculateIndicator rb
    putStrLn $ "EMA: " ++ show ema
    let rsi = calculateIndicator rb
    putStrLn $ "RSI: " ++ show rsi
    putStrLn "----- Dashboard -----"
    mapM_ print klines  -- Print Kline data
    putStrLn "---------------------"    


main :: IO ()
main = do
    let size = 16
    let rb = initRingBuffer size :: RingBuffer Kline
    connectToBuffer rb
    printDashboard rb
    printRingBufferContents rb