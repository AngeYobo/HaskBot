{-# LANGUAGE GADTs #-}
module SignalGenerator ( Thresholds(..), Signal(..), BuySignal(..), SellSignal(..), PriceData(..), MovingAverage(..), SignalHistory(..), recordSignal, isSMABullish, isRSIOversold, isSMABearish, isRSIOverbought, isGoldenCross, isDeathCross, generateSignals, calculateVolatility, isBullishCrossover, didSignalPersist) where
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import IndicatorEngine (calculateSMA, calculateEMA, calculateRSI)
import Data.Either (fromRight)
import DataProcessor (WebSocketMessage (..), KlineMessage(..), Kline(..), RingBuffer(..), GeneralMessage(..), addRingBuffer, initRingBuffer, processKlineData, getAllElements, queryRingBuffer, readDouble, printDashboard, closingPriceAbove, queryClosingPriceAbove, bufferSize, klineBufferRef, extractClosingPrices, getLatestMarketData, printRingBufferContents, printClosePrices)

-- Define the BuySignal type
data BuySignal = BuySignal 
    { buySymbol :: String
    , buyCurrency :: String
    , buyIndicator :: String 
    } deriving (Show, Eq)

-- Define the SellSignal type
data SellSignal = SellSignal 
    { sellSymbol :: String
    , sellCurrency :: String
    , sellIndicator :: String 
    } deriving (Show, Eq)

-- The Signal type with Buy, Sell, Hold, and StrongBuy options
data Signal = Buy BuySignal 
            | Sell SellSignal 
            | HoldSignal 
            | StrongBuySignal 
            deriving (Show, Eq)

-- Define the Thresholds type
data Thresholds = Thresholds
    { buyThreshold :: Double
    , sellThreshold :: Double
    , holdThreshold :: Double
    , strongBuyThreshold :: Double
    } deriving (Show, Eq)

-- Define the PriceData type
data PriceData = PriceData
    { date :: UTCTime
    , price :: Double
    } deriving (Show, Eq)

-- Define the MovingAverage type
data MovingAverage = SimpleMovingAverage [Double]
                   | ExponentialMovingAverage [Double]
                   deriving (Show, Eq)

-- Define the SignalHistory type
data SignalHistory = SignalHistory
    { signals :: [Signal]
    , timestamps :: [UTCTime]
    } deriving (Show, Eq)
-- Define the recordSignal function
recordSignal :: Signal -> SignalHistory -> IO SignalHistory
recordSignal sig history = do
    currentTime <- getCurrentTime
    return $ SignalHistory (signals history ++ [sig]) (timestamps history ++ [currentTime])
-- isGoldenCross, isDeathCross, and other function implementations...
-- Define the isGoldenCross function
isGoldenCross :: [Double] -> Int -> Int -> Bool
isGoldenCross prices shortTermPeriod longTermPeriod = do
    let shortTermSMA = calculateSMA prices shortTermPeriod
    let longTermSMA = calculateSMA prices longTermPeriod
    case (shortTermSMA, longTermSMA) of
        (Right stsma, Right ltsma) -> stsma > ltsma
        _ -> False
-- Define the isDeathCross function
isDeathCross :: [Double] -> Int -> Int -> Bool
isDeathCross prices shortTermPeriod longTermPeriod = do
    let shortTermSMA = calculateSMA prices shortTermPeriod
    let longTermSMA = calculateSMA prices longTermPeriod
    case (shortTermSMA, longTermSMA) of
        (Right stsma, Right ltsma) -> stsma < ltsma
        _ -> False
-- Define isSMABullish, isRSIOversold, etc. functions without IndicatorConfig
-- define the isSMABullish function
isSMABullish :: [Double] -> Bool
isSMABullish prices
    | null prices = False
    | otherwise = case calculateSMA prices 16 of
        Right sma -> last prices > sma
        Left _ -> False

isSMABearish :: [Double] -> Bool
isSMABearish prices
    | null prices = False
    | otherwise = case calculateSMA prices 16 of
        Right sma -> last prices < sma
        Left _ -> False

isRSIOversold :: [Double] -> Bool
isRSIOversold prices
    | null prices = False
    | otherwise = case calculateRSI prices 16 of
        Right rsi -> rsi < 30
        Left _ -> False
-- Only one implementation of isRSIOverbought should be present
isRSIOverbought :: [Double] -> Bool
isRSIOverbought prices
    | null prices = False
    | otherwise = case calculateRSI prices 16 of
        Right rsi -> rsi > 70
        Left _ -> False
    
-- Additional function to calculate volatility or other dynamic thresholds
calculateVolatility :: [Double] -> Double
calculateVolatility prices =
    let changes = zipWith (-) (tail prices) prices
        squaredChanges = map (^2) changes
        averageSquaredChange = sum squaredChanges / fromIntegral (length squaredChanges)
    in sqrt averageSquaredChange    

-- Function to determine if a bullish crossover occurred
isBullishCrossover :: [Double] -> [Double] -> Bool
isBullishCrossover shortTermPrices longTermPrices =
    let shortTermSMA = calculateSMA shortTermPrices 14
        longTermSMA = calculateSMA longTermPrices 30
    in case (shortTermSMA, longTermSMA) of
        (Right stsma, Right ltsma) -> stsma > ltsma
        _ -> False
-- Function to check if a signal persisted over time
didSignalPersist :: [Double] -> (Double -> Bool) -> Int -> Bool
didSignalPersist prices signalCondition periods =
    let recentPrices = take periods prices
        signals = map signalCondition recentPrices
    in and signals
-- Updated generateSignals function
generateSignals :: [Kline] -> IO [Signal]
generateSignals klines = do
    let prices = map close klines
    let requiredDataPoints = 16 -- Define the minimum number of data points required
    -- Ensure there are enough data points before generating signals
    if length prices >= requiredDataPoints then do
            let rsi = fromRight 0 $ calculateRSI prices 16
            let smaShort = fromRight 0 $ calculateSMA prices 15
            let smaLong = fromRight 0 $ calculateSMA prices 30
            let oversoldThreshold = 15  -- Dynamic based on volatility
            let overboughtThreshold = 93
            let smaSignal = if smaShort > smaLong
                            then Just $ Buy $ BuySignal "ADA" "USDT" "SMA Crossover"
                            else Nothing
            let rsiSignal
                    | rsi < oversoldThreshold
                    = Just $ Buy $ BuySignal "ADA" "USDT" "RSI Oversold"
                    | rsi > overboughtThreshold
                    = Just $ Sell $ SellSignal "ADA" "USDT" "RSI Overbought"
                    | otherwise = Nothing
            return $ catMaybes [smaSignal, rsiSignal]
        else
            return [] -- Return an empty list if there are not enough data points

