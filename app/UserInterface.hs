module UserInterface where

import IndicatorEngine (generateBuySignal, generateSellSignal)
import Options.Applicative
    ( fullDesc, help, info, long, switch, execParser, Parser )

data CLIOptions = CLIOptions
    { configureIndicators :: Bool
    , startBot :: Bool
    , stopBot :: Bool
    , viewAlerts :: Bool
    }

data Options = ConfigureOptions
             | StartOptions
             | StopOptions
             | AlertsOptions

data Alert = BuyAlert String
           | SellAlert String

cliParser :: Parser CLIOptions
cliParser = CLIOptions
    <$> switch (long "configure" <> help "Configure indicators and thresholds")
    <*> switch (long "start" <> help "Start the trading bot")
    <*> switch (long "stop" <> help "Stop the trading bot")
    <*> switch (long "alerts" <> help "View trading alerts")

main :: IO ()
main = do
    options <- execParser (info cliParser fullDesc)
    putStrLn "Processing options..."  
