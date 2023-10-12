module IndicatorEngineSpec where

import Test.Hspec ( hspec, describe, it, Spec, shouldBe )
import IndicatorEngine ( calculateSMA, calculateEMA, calculateRSI, )
import DataFetcher (CandlestickData(..))

-- Function to round a Maybe Double value to 2 decimal places
roundMaybe :: Maybe Double -> Maybe Double
roundMaybe Nothing = Nothing
roundMaybe (Just x) = Just (fromIntegral (round (x * 100)) / 100)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "calculateSMA" $ do
        it "calculates SMA correctly" $ do
            let prices = [10.0, 15.0, 20.0, 25.0, 30.0]  -- Sufficient data for SMA
                period = 3
                expectedSMA = Just 25.0
                resultSMA = roundMaybe (calculateSMA prices period)
            resultSMA `shouldBe` expectedSMA
            putStrLn $ "Expected SMA: " ++ show expectedSMA
            putStrLn $ "Result SMA: " ++ show resultSMA
            resultSMA `shouldBe` expectedSMA

        it "handles insufficient data for SMA calculation" $ do
            let prices = [10.0, 15.0]  -- Insufficient data for SMA (period=3)
                period = 3
                resultSMA = calculateSMA prices period
            resultSMA `shouldBe` Nothing

    describe "calculateEMA" $ do
        it "calculates EMA correctly" $ do
            let prices = [10.0, 15.0, 20.0, 25.0, 30.0]  -- Sufficient data for EMA
                period = 3
                expectedEMA = Just 26.67
                resultEMA = roundMaybe (calculateEMA prices period)
            resultEMA `shouldBe` expectedEMA

        it "handles insufficient data for EMA calculation" $ do
            let prices = [10.0, 15.0]  -- Insufficient data for EMA (period=3)
                period = 3
                resultEMA = calculateEMA prices period
            resultEMA `shouldBe` Nothing

    describe "calculateRSI" $ do
        it "calculates RSI correctly" $ do
            let prices = [10.0, 15.0, 10.0, 20.0, 30.0]  -- Sufficient data for RSI
                period = 3
                expectedRSI = Just 70.97
                resultRSI = roundMaybe (calculateRSI prices period)
            resultRSI `shouldBe` expectedRSI

        it "handles insufficient data for RSI calculation" $ do
            let prices = [10.0, 15.0]  -- Insufficient data for RSI (period=3)
                period = 3
                resultRSI = calculateRSI prices period
            resultRSI `shouldBe` Nothing
