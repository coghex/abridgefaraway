{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_abridgefaraway (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/fatty/.cabal/bin"
libdir     = "/home/fatty/.cabal/lib/x86_64-linux-ghc-8.0.1/abridgefaraway-0.1.0.0"
datadir    = "/home/fatty/.cabal/share/x86_64-linux-ghc-8.0.1/abridgefaraway-0.1.0.0"
libexecdir = "/home/fatty/.cabal/libexec"
sysconfdir = "/home/fatty/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "abridgefaraway_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "abridgefaraway_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "abridgefaraway_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "abridgefaraway_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "abridgefaraway_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
