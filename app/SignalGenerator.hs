-- SignalGenerator.hs

module SignalGenerator 
    ( Thresholds(..)
    , generateBuySignals
    , generateSellSignals
    , Signal(..)         
    , BuySignal(..)      
    , SellSignal(..)     
    , PriceData(..)     
    , MovingAverage(..)  
    , SignalHistory(..)  
    , recordSignal       
    
    )
where

import Data.Time.Clock (UTCTime, getCurrentTime)

data Signal = Buy BuySignal 
    | Sell SellSignal 
    | HoldSignal 
    | StrongBuySignal deriving (Show, Eq)

data BuySignal = BuySignal 
    { buySymbol :: String
    , buyCurrency :: String
    , buyIndicator :: String 
    } deriving (Show, Eq)

data SellSignal = SellSignal 
    { sellSymbol :: String
    , sellCurrency :: String
    , sellIndicator :: String 
    } deriving (Show, Eq)

data Thresholds = Thresholds
    { buyThreshold :: Double
    , sellThreshold :: Double
    , holdThreshold :: Double
    , strongBuyThreshold :: Double
    } deriving (Show, Eq)

data PriceData = PriceData
    { date :: UTCTime
    , price :: Double
    } deriving (Show, Eq)

data MovingAverage = SimpleMovingAverage [Double] 
    | ExponentialMovingAverage [Double] deriving (Show, Eq)

data SignalHistory = SignalHistory
    { signals :: [Signal]
    , timestamps :: [UTCTime]
    } deriving (Show, Eq)

recordSignal :: Signal -> SignalHistory -> IO SignalHistory
recordSignal sig history = do
    currentTime <- getCurrentTime
    return $ SignalHistory (signals history ++ [sig]) (timestamps history ++ [currentTime])

generateBuySignals :: [Double] -> Double -> [Bool]
generateBuySignals rsiData threshold = map (< threshold) rsiData

generateSellSignals :: [Double] -> Double -> [Bool]
generateSellSignals rsiData threshold = map (> threshold) rsiData

