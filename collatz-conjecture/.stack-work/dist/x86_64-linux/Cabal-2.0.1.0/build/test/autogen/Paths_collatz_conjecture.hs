{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_collatz_conjecture (
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
version = Version [1,2,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/bin"
libdir     = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/lib/x86_64-linux-ghc-8.2.2/collatz-conjecture-1.2.0.2-IL2E2KPjWjUAtbj5ncUMYR-test"
dynlibdir  = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/share/x86_64-linux-ghc-8.2.2/collatz-conjecture-1.2.0.2"
libexecdir = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/libexec/x86_64-linux-ghc-8.2.2/collatz-conjecture-1.2.0.2"
sysconfdir = "/home/ninja/exercism/haskell/collatz-conjecture/.stack-work/install/x86_64-linux/lts-11.1/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "collatz_conjecture_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "collatz_conjecture_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "collatz_conjecture_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "collatz_conjecture_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "collatz_conjecture_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "collatz_conjecture_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
