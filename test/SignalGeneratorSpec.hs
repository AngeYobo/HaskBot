-- test/SignalGeneratorSpec.hs
module SignalGeneratorSpec where

import Test.Hspec
import SignalGenerator ( Thresholds(..)
    , generateBuySignals
    , generateSellSignals
    , Signal(..) -- Export Signal data constructor
    , BuySignal(..)  -- Export BuySignal data constructor
    , SellSignal(..) -- Export SellSignal data constructor
    )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "generateBuySignals" $ do
        it "generates buy signals correctly" $ do
            let rsiData = [70, 60, 50, 40, 30]
            let threshold = 60
            let expectedSignals = [False, True, True, True, True]
            generateBuySignals rsiData threshold `shouldBe` expectedSignals

        -- Add more test cases for generateBuySignals as needed

    describe "generateSellSignals" $ do
        it "generates sell signals correctly" $ do
            let rsiData = [30, 40, 50, 60, 70]
            let threshold = 60
            let expectedSignals = [False, False, False, False, True]
            generateSellSignals rsiData threshold `shouldBe` expectedSignals

        -- Add more test cases for generateSellSignals as needed

    -- Add more describe blocks and test cases for other SignalGenerator functions

    -- Example describe block for a new function
    --describe "yourNewFunction" $ do
        --it "should behave correctly in a specific scenario" $ do
            -- Define your test scenario and expected outcomes
            -- Call your function and assert the results
            -- Use `shouldBe` or other appropriate assertions

        -- Add more test cases for yourNewFunction as needed
