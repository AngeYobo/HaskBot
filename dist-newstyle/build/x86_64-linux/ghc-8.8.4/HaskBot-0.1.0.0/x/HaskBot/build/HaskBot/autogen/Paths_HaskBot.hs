{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HaskBot (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ange/.cabal/bin"
libdir     = "/home/ange/.cabal/lib/x86_64-linux-ghc-8.8.4/HaskBot-0.1.0.0-inplace-HaskBot"
dynlibdir  = "/home/ange/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ange/.cabal/share/x86_64-linux-ghc-8.8.4/HaskBot-0.1.0.0"
libexecdir = "/home/ange/.cabal/libexec/x86_64-linux-ghc-8.8.4/HaskBot-0.1.0.0"
sysconfdir = "/home/ange/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskBot_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskBot_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "HaskBot_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "HaskBot_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskBot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskBot_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
