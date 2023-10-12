module DataFetcherSpec (spec) where  -- Export the 'spec' function

import Test.Hspec( describe, it, expectationFailure, shouldBe, Spec )
import Control.Exception (SomeException, try)
import DataFetcher (fetchCandlestickData)

spec :: Spec
spec = do
    describe "fetchCandlestickData" $ do
        it "fetches candlestick data without errors" $ do
            result <- try fetchCandlestickData
            case result of
                Left ex -> do
                    let _ = ex :: SomeException
                    expectationFailure $ "Test failed with exception: " ++ show ex
                Right response -> do
                    null response `shouldBe` False
