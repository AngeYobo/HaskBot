{-# LANGUAGE ScopedTypeVariables #-}

module DisplayManagerTest (spec) where

import Test.Hspec
import DisplayManager
import SignalGenerator (Signal(..), BuySignal(..), SellSignal(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import DataProcessor (Kline(..))
import Data.Text (pack)

spec :: Spec
spec = do
  describe "DisplayManager" $ do
    it "displays Kline data correctly" $ do
      let sampleKline = Kline
            { start = UTCTime (fromGregorian 2024 6 29) (secondsToDiffTime 0)
            , end = UTCTime (fromGregorian 2024 6 29) (secondsToDiffTime 60)
            , interval = pack "1"
            , open = 0.3905
            , close = 0.3907
            , high = 0.3907
            , low = 0.3905
            , volume = 114980.0
            , turnover = 44916.5028
            , confirm = False
            , timestamp = UTCTime (fromGregorian 2024 6 29) (secondsToDiffTime 42)
            }
      let output = displayKline sampleKline
      output `shouldContain` "Open Time: 2024-06-29 00:00:00 UTC"
      output `shouldContain` "Open: 0.3905"
      output `shouldContain` "High: 0.3907"
      output `shouldContain` "Low: 0.3905"
      output `shouldContain` "Close: 0.3907"
      output `shouldContain` "Volume: 114980.0"

    it "displays Signal correctly" $ do
      let buySignal = Buy $ BuySignal "ADA" "USDT" "SMA Crossover"
      let sellSignal = Sell $ SellSignal "ADA" "USDT" "RSI Overbought"
      displaySignal buySignal `shouldBe` "Signal: Buy (BuySignal {buySymbol = \"ADA\", buyCurrency = \"USDT\", buyIndicator = \"SMA Crossover\"})"
      displaySignal sellSignal `shouldBe` "Signal: Sell (SellSignal {sellSymbol = \"ADA\", sellCurrency = \"USDT\", sellIndicator = \"RSI Overbought\"})"
