{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_monopoly (
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

bindir     = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\bin"
libdir     = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\lib\\x86_64-windows-ghc-8.10.4\\monopoly-0.1.0.0-75qQITSfXfJIMCfZc8r2gZ-monopoly-exe"
dynlibdir  = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\share\\x86_64-windows-ghc-8.10.4\\monopoly-0.1.0.0"
libexecdir = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\libexec\\x86_64-windows-ghc-8.10.4\\monopoly-0.1.0.0"
sysconfdir = "C:\\Users\\alexi\\desktop\\pdp\\monopoly\\monopoly\\.stack-work\\install\\60019f5b\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "monopoly_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "monopoly_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "monopoly_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "monopoly_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "monopoly_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "monopoly_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
