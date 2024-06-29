{-# LANGUAGE ScopedTypeVariables #-}

module SignalGeneratorTest (spec) where

import Test.Hspec
import SignalGenerator
import DataProcessor (Kline(..))
import Data.Time.Clock (UTCTime, secondsToDiffTime, UTCTime(..))
import Data.Text (pack)

-- Helper function to create a UTCTime for testing
testTime :: UTCTime
testTime = UTCTime (toEnum 0) (secondsToDiffTime 0)

-- Sample Kline data for testing
testKline :: Kline
testKline = Kline testTime testTime (pack "") 1.0 2.0 3.0 1.0 100.0 200.0 True testTime

spec :: Spec
spec = do
  describe "SignalGenerator" $ do
    it "generates buy signals correctly" $ do
      let klines = [testKline]
      let expected = [Buy $ BuySignal "TAO" "USDT" "SMA Crossover"]
      signals <- generateSignals klines
      signals `shouldBe` expected

    it "detects a Golden Cross correctly" $ do
      let shortTermPrices = [1, 2, 3, 4, 5, 6]
      let longTermPrices = [2, 3, 4, 5, 5, 5]
      isGoldenCross shortTermPrices 3 5 `shouldBe` True

    it "detects a Death Cross correctly" $ do
      let shortTermPrices = [6, 5, 4, 3, 2, 1]
      let longTermPrices = [5, 5, 5, 4, 3, 2]
      isDeathCross shortTermPrices 3 5 `shouldBe` True

    it "detects an SMA Bullish condition correctly" $ do
      let prices = [1, 2, 3, 4, 5, 6]
      isSMABullish prices `shouldBe` True

    it "detects RSI Oversold correctly" $ do
      let prices = [70, 69, 68, 67, 66, 65]
      isRSIOversold prices `shouldBe` True

    it "detects RSI Overbought correctly" $ do
      let prices = [30, 31, 32, 33, 34, 35]
      isRSIOverbought prices `shouldBe` True
