  module Main where

  import Test.Hspec ( hspec, describe, it )
  import IndicatorEngineSpec ( spec )
  import DataFetcherSpec (spec )  -- Import and alias the DataFetcherSpec module here
  import WebSocketConnectorSpec ( spec )
  import UserInterfaceSpec (spec)
  import qualified Text.Read.Lex as Main
  import SignalGeneratorSpec (spec) 
    
  main :: IO ()
  main = hspec $ do

      DataFetcherSpec.spec
      IndicatorEngineSpec.spec
      SignalGenerator.spec
      UserInterfaceSpec.spec      
      WebSocketConnectorSpec.spec
      
    