{-# LANGUAGE OverloadedStrings #-}

module WebSocketConnector
    ( WSConnectionStatus(..)
    , WSMessage(..)
    , connectToWebSocket
    , handleTextMessage
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import DataFetcher ()

data WSConnectionStatus = Connected | Disconnected deriving (Eq, Show)

newtype WSMessage = WSMessage Text deriving (Eq, Show)

connectToWebSocket :: Text   
                   -> IO WSConnectionStatus
connectToWebSocket serverUrl = do
    putStrLn $ "Connecting to WebSocket server: " ++ show serverUrl
    WS.runClient (Data.Text.unpack serverUrl) 80 "/" $ \conn -> do
        putStrLn "Connected!"
        _ <- forkIO $ handleWebSocketMessages conn
        return Connected


handleWebSocketMessages :: WS.Connection -> IO ()
handleWebSocketMessages conn = do
    forever $ do
        msg <- WS.receiveData conn
        handleTextMessage msg

handleTextMessage :: Text -> IO ()
handleTextMessage message = do
    putStrLn $ "Received WebSocket message: " ++ show message

main :: IO ()
main = do
    status <- connectToWebSocket "wss://stream.binance.com:9443/ws/btcusdt@trade"
    putStrLn $ "Connection status: " ++ show status
    forever $ threadDelay 1000000 
