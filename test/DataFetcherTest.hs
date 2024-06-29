{-# LANGUAGE ScopedTypeVariables #-}

module DataFetcherTest (spec) where

import Test.Hspec
import DataFetcher
import Control.Concurrent
import Control.Exception
import Data.IORef
import DataProcessor (RingBuffer, initRingBuffer)
import qualified DataProcessor as DP
import Control.Monad (void)
import Network.WebSockets (runClient)
import Wuss (runSecureClient)
import Control.Monad.IO.Class (liftIO)
import qualified Network.WebSockets as WS
import Network.Socket (PortNumber)

spec :: Spec
spec = do
  describe "DataFetcher" $ do
    connectionAttemptsTests
    resetConnectionAttemptsTests
    checkConnectionLimitTests
    attemptConnectionTests
    waitForConnectionLimitResetTests

connectionAttemptsTests :: Spec
connectionAttemptsTests = describe "connectionAttempts" $ do
  it "initializes connection attempts to 0" $ do
    attemptsRef <- connectionAttempts
    attempts <- readIORef attemptsRef
    attempts `shouldBe` 0

resetConnectionAttemptsTests :: Spec
resetConnectionAttemptsTests = describe "resetConnectionAttempts" $ do
  it "resets connection attempts to 0 after 5 minutes" $ do
    attemptsRef <- connectionAttempts
    writeIORef attemptsRef 100

    void $ forkIO $ resetConnectionAttempts attemptsRef

    threadDelay (10 * 1000000) -- 10 seconds for the test instead of 5 minutes

    attempts <- readIORef attemptsRef
    attempts `shouldBe` 0

checkConnectionLimitTests :: Spec
checkConnectionLimitTests = describe "checkConnectionLimit" $ do
  it "returns True when under the limit" $ do
    attemptsRef <- connectionAttempts
    writeIORef attemptsRef 499
    underLimit <- checkConnectionLimit attemptsRef
    underLimit `shouldBe` True

  it "returns False when at or over the limit" $ do
    attemptsRef <- connectionAttempts
    writeIORef attemptsRef 500
    overLimit <- checkConnectionLimit attemptsRef
    overLimit `shouldBe` False

attemptConnectionTests :: Spec
attemptConnectionTests = describe "attemptConnection" $ do
  it "increments connection attempts on failure" $ do
    attemptsRef <- connectionAttempts  -- Initialize connection attempts
    klineBufferRef <- newIORef (initRingBuffer DP.bufferSize)  -- Initialize a dummy RingBuffer
    
    -- Mock runSecureClient to always fail
    let mockRunSecureClient :: String -> PortNumber -> String -> (WS.Connection -> IO ()) -> IO ()
        mockRunSecureClient _ _ _ _ = throwIO (userError "Connection failed")

    let originalRunSecureClient = runSecureClient
    (runSecureClient :: String -> PortNumber -> String -> (WS.Connection -> IO ()) -> IO ()) <- return mockRunSecureClient
    
    attemptConnection attemptsRef klineBufferRef  -- Attempt the connection
    
    -- Restore the original function
    (runSecureClient :: String -> PortNumber -> String -> (WS.Connection -> IO ()) -> IO ()) <- return originalRunSecureClient
    
    -- After the connection attempt, check if the attempts have been incremented.
    attemptsAfter <- readIORef attemptsRef
    attemptsAfter `shouldBe` 1  -- Assuming this was the first attempt and it failed.

waitForConnectionLimitResetTests :: Spec
waitForConnectionLimitResetTests = describe "waitForConnectionLimitReset" $ do
  it "waits for connection limit to reset when over limit" $ do
    attemptsRef <- connectionAttempts
    writeIORef attemptsRef 501  -- Set attempts over the limit

    -- Run waitForConnectionLimitReset in a separate thread
    done <- newEmptyMVar
    forkIO $ do
      waitForConnectionLimitReset attemptsRef
      putMVar done ()

    -- Simulate time passing
    threadDelay (60 * 1000000)  -- 60 seconds for the test
    
    -- Check if the attempts have been reset
    allowed <- checkConnectionLimit attemptsRef
    allowed `shouldBe` True

    -- Ensure the thread completes
    takeMVar done
