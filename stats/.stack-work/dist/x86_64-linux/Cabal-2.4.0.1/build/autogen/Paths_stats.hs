{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_stats (
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

bindir     = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/bin"
libdir     = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/lib/x86_64-linux-ghc-8.6.5/stats-0.1.0.0-9SWD760m80PHyVBihtycdF"
dynlibdir  = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/share/x86_64-linux-ghc-8.6.5/stats-0.1.0.0"
libexecdir = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/libexec/x86_64-linux-ghc-8.6.5/stats-0.1.0.0"
sysconfdir = "/home/jovyan/work/.stack-work/install/x86_64-linux/807fbed5252de80f382023b2168edf7a9cc2980b60bb9d6c772ac5e1b6530ee2/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stats_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stats_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "stats_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "stats_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stats_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stats_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
