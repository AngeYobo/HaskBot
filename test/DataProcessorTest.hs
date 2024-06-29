{-# LANGUAGE ScopedTypeVariables #-}

module DataProcessorTest (spec) where

import Test.Hspec
import DataProcessor
    ( RingBuffer(maxSize, buffer, index),
      Kline(Kline),
      initRingBuffer,
      addRingBuffer,
      getAllElements,
      queryRingBuffer,
      closingPriceAbove )
import Data.Time.Clock (UTCTime, secondsToDiffTime, UTCTime(..))
import qualified Data.Vector as V
import Data.Text (Text, pack)

-- Helper function to create a UTCTime for testing
testTime :: UTCTime
testTime = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- Sample Kline data for testing
testKline :: Kline
testKline = Kline testTime testTime (pack "") 1.0 2.0 3.0 1.0 100.0 200.0 True testTime

spec :: Spec
spec = do
  describe "RingBuffer" $ do
    it "initializes correctly" $ do
      let rb = initRingBuffer 10 :: RingBuffer Kline
      maxSize rb `shouldBe` 10
      index rb `shouldBe` 0
      buffer rb `shouldBe` V.replicate 10 Nothing

    it "adds an element correctly" $ do
      let rb = initRingBuffer 1 :: RingBuffer Kline
      let rb' = addRingBuffer rb testKline
      V.head (buffer rb') `shouldBe` Just testKline
      index rb' `shouldBe` 0  -- Index should wrap around to 0 in a 1-size buffer

    it "retrieves all elements correctly" $ do
      let rb = initRingBuffer 2 :: RingBuffer Kline
      let rb' = addRingBuffer (addRingBuffer rb testKline) testKline
      let elements = getAllElements rb'
      elements `shouldBe` [testKline, testKline]

    it "queries elements correctly" $ do
      let rb = initRingBuffer 2 :: RingBuffer Kline
      let rb' = addRingBuffer rb testKline  -- Add a single Kline
      let queried = queryRingBuffer (closingPriceAbove 1.5) rb'
      queried `shouldBe` [testKline]
