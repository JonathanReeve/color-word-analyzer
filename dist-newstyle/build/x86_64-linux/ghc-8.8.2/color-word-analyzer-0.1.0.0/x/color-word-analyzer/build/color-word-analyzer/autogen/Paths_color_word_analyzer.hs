{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_color_word_analyzer (
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

bindir     = "/home/jon/.cabal/bin"
libdir     = "/home/jon/.cabal/lib/x86_64-linux-ghc-8.8.2/color-word-analyzer-0.1.0.0-inplace-color-word-analyzer"
dynlibdir  = "/home/jon/.cabal/lib/x86_64-linux-ghc-8.8.2"
datadir    = "/home/jon/.cabal/share/x86_64-linux-ghc-8.8.2/color-word-analyzer-0.1.0.0"
libexecdir = "/home/jon/.cabal/libexec/x86_64-linux-ghc-8.8.2/color-word-analyzer-0.1.0.0"
sysconfdir = "/home/jon/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "color_word_analyzer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "color_word_analyzer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "color_word_analyzer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "color_word_analyzer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "color_word_analyzer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "color_word_analyzer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
