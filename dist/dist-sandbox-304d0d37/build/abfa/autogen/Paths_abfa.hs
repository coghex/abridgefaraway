{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_abfa (
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
version = Version [2,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/hoest/work/abfa/.cabal-sandbox/bin"
libdir     = "/home/hoest/work/abfa/.cabal-sandbox/lib/x86_64-linux-ghc-8.4.3/abfa-2.0.0.0-Kts12Tu2HS5CW5KV2xY6UJ"
dynlibdir  = "/home/hoest/work/abfa/.cabal-sandbox/lib/x86_64-linux-ghc-8.4.3"
datadir    = "/home/hoest/work/abfa/.cabal-sandbox/share/x86_64-linux-ghc-8.4.3/abfa-2.0.0.0"
libexecdir = "/home/hoest/work/abfa/.cabal-sandbox/libexec/x86_64-linux-ghc-8.4.3/abfa-2.0.0.0"
sysconfdir = "/home/hoest/work/abfa/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "abfa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "abfa_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "abfa_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "abfa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "abfa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "abfa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
