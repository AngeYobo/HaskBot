{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module DataFetcher
    ( fetchCandlestickData
    , formatCandlestickData
    , CandlestickData(..) 
    , closePrice
    ) where 

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock ( UTCTime, getCurrentTime, addUTCTime, NominalDiffTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Aeson (FromJSON, parseJSON, eitherDecode, (.:), Value(..), withObject, withArray)  -- Add this line
import Data.Aeson.Types ( FromJSON(parseJSON), (.:), withObject, Parser)
import Data.List (intercalate)
import Data.Foldable (mapM_)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString)
import Control.Exception (try, SomeException)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client ( newManager, parseRequest, responseBody, Request(..), RequestBody(..), responseStatus, Response, httpLbs )
import Network.HTTP.Simple (setRequestHeaders, httpJSON)
import Network.HTTP.Types (HeaderName)
import Network.HTTP.Types.Header (hUserAgent, hAuthorization)
import System.Environment (getEnv)


-- Here I define my API key and secret inside my Env Variable PC to keep it away from anyone. Very importatnt for security !!!
apiKeyEnvVarName :: String
apiKeyEnvVarName = "BINANCE_API_KEY"

apiSecretEnvVarName :: String
apiSecretEnvVarName = "BINANCE_API_SECRET"

-- Let's retrieve the API key and secret from environment variables
apiKey :: IO Text
apiKey = pack <$> getEnv apiKeyEnvVarName

apiSecret :: IO Text
apiSecret = pack <$> getEnv apiSecretEnvVarName

-- let's define the Binance API base URL
baseUrl :: String
baseUrl = "https://api.binance.com"

-- let's define the interval for candlestick data (e.g., 1 hour)
candlestickInterval :: NominalDiffTime
candlestickInterval = 3600

-- let's define the data type for BTCUSDT price data
data CandlestickData = CandlestickData
    { openTime :: Integer,
      openPrice :: Double,
      highPrice :: Double,
      lowPrice :: Double,
      --closePrice :: Double, -- Extracted it for calculation in module Main
      volume :: Double
    } deriving (Show, Generic)    

instance FromJSON CandlestickData where
    parseJSON :: Value -> Parser CandlestickData
    parseJSON = withArray "CandlestickData" $ \arr -> do                
        if V.length arr >= 6
            then do
                openTime' <- parseJSON (arr V.! 0)
                openPrice' <- parseJSON (arr V.! 1)
                highPrice' <- parseJSON (arr V.! 2)
                lowPrice' <- parseJSON (arr V.! 3)
                --closePrice' <- parseJSON (arr V.! 4)
                volume' <- parseJSON (arr V.! 5)
                return CandlestickData
                    { openTime = openTime'
                    , openPrice = read openPrice'
                    , highPrice = read highPrice'
                    , lowPrice = read lowPrice'
                    --, closePrice = read closePrice'
                    , volume = read volume'
                    }
            else fail "At least 6 elements of an array are expected !"
            
-- let's define closePrice function to extract the close price from CandlestickData
closePrice :: CandlestickData -> Double
closePrice (CandlestickData _ _ _ close _) = close

-- let's create a function to fetch candlestick data
fetchCandlestickData :: IO [CandlestickData]
fetchCandlestickData = do
    -- Here i generate the request URL for candlestick data
    currentTime <- getCurrentTime
    let startTime = addUTCTime (-candlestickInterval * 10) currentTime
    let endTime = currentTime
    let symbol = "BTCUSDT"
    let interval = "1h"
    let url = baseUrl ++ "/api/v3/klines?symbol=" ++ symbol ++ "&interval=" ++ interval ++ "&startTime=" ++ show (utctTimeToMillis startTime) ++ "&endTime=" ++ show (utctTimeToMillis endTime)

    -- let's define create a new HTTP manager
    manager <- liftIO $ newManager tlsManagerSettings

    -- let's define create an HTTP request with headers (including user agent and API key)
    apiKeyValue <- apiKey
    apiSecretValue <- apiSecret
    apiKeyValue <- apiKey
    apiSecretValue <- apiSecret
    let requestHeaders = [(hUserAgent, encodeUtf8 (pack "HaskBot")), (hAuthorization, encodeUtf8 apiKeyValue)]

    -- We need here to convert the list to RequestHeaders
    let headers = map (\(name, value) -> (name, value)) requestHeaders

    -- also specify the type of initialRequest as Request
    initialRequest <- liftIO $ parseRequest url

    -- And now i can use the headers in the request
    let request = initialRequest { requestHeaders = headers }

    -- Here I'm making the HTTP request and also handling exceptions
    response <- liftIO $ try $ httpLbs request manager :: IO (Either SomeException (Response LBS.ByteString))
    case response of
        Left ex -> do
            putStrLn $ "Exception: " ++ show ex
            return []
        Right r -> do
            -- Here I print the response body for debugging
            LBS.putStrLn $ responseBody r
            let body = responseBody r
            case eitherDecode body of
                Left err -> do
                    putStrLn $ "Error parsing response: " ++ err
                    return []
                Right responseData -> do
                    putStrLn "Received data:"
                    mapM_ (putStrLn . formatCandlestickData) responseData
                    return responseData

-- I use a Helper function to convert UTCTime to milliseconds
utctTimeToMillis :: UTCTime -> Integer
utctTimeToMillis = round . (* 1000) . utcTimeToPOSIXSeconds

-- Now let's Format CandlestickData as a string for humain
formatCandlestickData :: CandlestickData -> String
formatCandlestickData dataItem =
    intercalate " | " [ "Open Time: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (posixSecondsToUTCTime (fromIntegral $ openTime dataItem `div` 1000) :: UTCTime),
                        "Open Price: " ++ show (openPrice dataItem),
                        "High Price: " ++ show (highPrice dataItem),
                        "Low Price: " ++ show (lowPrice dataItem),
                        "Close Price: " ++ show (closePrice dataItem),
                        "Volume: " ++ show (volume dataItem)]
