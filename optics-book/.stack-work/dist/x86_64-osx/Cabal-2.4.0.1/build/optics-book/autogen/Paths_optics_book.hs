{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_optics_book (
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

bindir     = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/bin"
libdir     = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/lib/x86_64-osx-ghc-8.6.5/optics-book-0.1.0.0-9gH750F9BsrnmyDh8KRMq-optics-book"
dynlibdir  = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/share/x86_64-osx-ghc-8.6.5/optics-book-0.1.0.0"
libexecdir = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/libexec/x86_64-osx-ghc-8.6.5/optics-book-0.1.0.0"
sysconfdir = "/Users/139137/developer/src/personal/notebooks/.stack-work/install/x86_64-osx/35d8119b2f0780cce25b3f41ff74c1e8832b09c806cba1f0bfd907c2d551b62d/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "optics_book_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "optics_book_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "optics_book_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "optics_book_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "optics_book_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "optics_book_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
