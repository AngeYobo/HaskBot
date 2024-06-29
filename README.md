
# HaskBot

HaskBot is a Haskell-based trading bot that connects to cryptocurrency exchange APIs, processes market data, and generates trading signals based on technical indicators. The bot is modular and includes various components such as data fetching, data processing, indicator calculation, and signal generation.

## Features

- **Data Fetching**: Connects to WebSocket APIs to fetch real-time market data.
- **Data Processing**: Handles incoming market data, storing it in a ring buffer for efficient access.
- **Technical Indicators**: Calculates various technical indicators such as SMA, EMA, and RSI.
- **Signal Generation**: Generates trading signals based on predefined strategies.
- **Dashboard**: Displays real-time market data, indicators, and trading signals.

## Modules

- **DataFetcher**: Handles the WebSocket connection and data fetching.
- **DataProcessor**: Processes incoming market data and manages the ring buffer.
- **IndicatorEngine**: Calculates technical indicators.
- **SignalGenerator**: Generates trading signals based on technical indicators.
- **DisplayManager**: Displays market data, indicators, and trading signals.

## Installation

To get started with HaskBot, follow these steps:

### Prerequisites

- [GHC (The Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal (The Haskell Package Manager)](https://www.haskell.org/cabal/)
- Required system libraries: `pkg-config`, `zlib`

### Clone the Repository

```bash
git clone https://github.com/your-username/HaskBot.git
cd HaskBot
```

### Install Dependencies

```bash
cabal update
cabal install --only-dependencies
```

### Build the Project

```bash
cabal build
```

## Usage

To run the bot:

```bash
cabal run HaskBot
```

To run the tests:

```bash
cabal test
```

## Project Structure

```plaintext
HaskBot/
├── app/
│   └── Main.hs
├── src/
│   ├── DataFetcher.hs
│   ├── DataProcessor.hs
│   ├── DisplayManager.hs
│   ├── IndicatorEngine.hs
│   └── SignalGenerator.hs
├── test/
│   ├── DataFetcherTest.hs
│   ├── DataProcessorTest.hs
│   ├── DisplayManagerTest.hs
│   ├── IndicatorEngineTest.hs
│   ├── MainTest.hs
│   └── SignalGeneratorTest.hs
├── HaskBot.cabal
├── LICENSE
└── README.md
```

## Contributing

Contributions are welcome! Please open an issue or submit a pull request with any improvements or bug fixes.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

For any inquiries, please contact Ange Yobo at angeyobo@gmail.com.



This README file includes sections on features, installation, usage, project structure, contributing, license, and contact information. 
Adjust the repository URL and contact information as necessary. 
This should provide a comprehensive overview and guide for anyone interested in using or contributing to HaskBot.