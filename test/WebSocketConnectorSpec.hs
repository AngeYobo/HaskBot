{-# LANGUAGE OverloadedStrings #-}

module WebSocketConnectorSpec where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )
import WebSocketConnector
    ( WSConnectionStatus(Disconnected, Connected), connectToWebSocket, handleTextMessage)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "connectToWebSocket" $ do
        it "connects to a WebSocket server" $ do
            -- Replace with your WebSocket server URL
            let serverUrl = "wss://example.com"
            status <- connectToWebSocket serverUrl
            status `shouldBe` Connected

        it "handles WebSocket disconnection" $ do
            -- Replace with a WebSocket server URL that doesn't exist
            let serverUrl = "wss://nonexistent.com"
            status <- connectToWebSocket serverUrl
            status `shouldBe` Disconnected

    
