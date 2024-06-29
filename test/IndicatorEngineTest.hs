{-# LANGUAGE OverloadedStrings #-}
module IndicatorEngineTest (spec) where

import Test.Hspec
import IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)

spec :: Spec
spec = do
  smaTests
  emaTests
  rsiTests

smaTests :: Spec
smaTests = describe "Simple Moving Average (SMA) Calculation" $ do
  it "correctly calculates SMA for a given period" $ do
    calculateSMA [10, 20, 30, 40, 50] 3 `shouldBe` Right 40.0
  it "returns an error for insufficient data" $ do
    calculateSMA [10, 20] 3 `shouldBe` Left "Not enough data to calculate SMA"
  it "returns an error for a non-positive period" $ do
    calculateSMA [10, 20, 30] 0 `shouldBe` Left "Period must be positive"

emaTests :: Spec
emaTests = describe "Exponential Moving Average (EMA) Calculation" $ do
  it "correctly calculates EMA for a given period" $ do
    calculateEMA [10, 20, 30, 40, 50] 3 `shouldBe` Right 36.666666666666664
  it "returns an error for insufficient data" $ do
    calculateEMA [10, 20] 3 `shouldBe` Left "Not enough data to calculate EMA"
  it "returns an error for a non-positive period" $ do
    calculateEMA [10, 20, 30] 0 `shouldBe` Left "Period must be positive"

rsiTests :: Spec
rsiTests = describe "Relative Strength Index (RSI) Calculation" $ do
  it "correctly calculates RSI for a given period" $ do
    let prices = [45, 46, 45, 48, 49, 50, 51, 50, 49, 48, 47, 46, 47, 48, 49, 50, 51, 52, 53, 54]
    calculateRSI prices 14 `shouldBe` Right 70.53571428571429
  it "returns an error for insufficient data" $ do
    calculateRSI [10, 20, 30, 40, 50] 6 `shouldBe` Left "Not enough data to calculate RSI"
  it "returns an error for a non-positive period" $ do
    calculateRSI [10, 20, 30, 40, 50] 0 `shouldBe` Left "Period must be positive"

-- Additional test cases and helper functions can be added here
