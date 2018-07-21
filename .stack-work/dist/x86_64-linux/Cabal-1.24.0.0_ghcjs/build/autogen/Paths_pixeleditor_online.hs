{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_pixeleditor_online (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/lamb/Documents/pixeditor-online/.stack-work/install/x86_64-linux/lts-6.20/ghcjs-0.2.0.9006020_ghc-7.10.3/bin"
libdir     = "/home/lamb/Documents/pixeditor-online/.stack-work/install/x86_64-linux/lts-6.20/ghcjs-0.2.0.9006020_ghc-7.10.3/lib/x86_64-linux-ghcjs-0.2.0.9006020-ghc7_10_3/pixeleditor-online-0.1.0.0-32xJMPkKPmZ6s7aHeBmhb5"
datadir    = "/home/lamb/Documents/pixeditor-online/.stack-work/install/x86_64-linux/lts-6.20/ghcjs-0.2.0.9006020_ghc-7.10.3/share/x86_64-linux-ghcjs-0.2.0.9006020-ghc7_10_3/pixeleditor-online-0.1.0.0"
libexecdir = "/home/lamb/Documents/pixeditor-online/.stack-work/install/x86_64-linux/lts-6.20/ghcjs-0.2.0.9006020_ghc-7.10.3/libexec"
sysconfdir = "/home/lamb/Documents/pixeditor-online/.stack-work/install/x86_64-linux/lts-6.20/ghcjs-0.2.0.9006020_ghc-7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pixeleditor_online_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pixeleditor_online_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "pixeleditor_online_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pixeleditor_online_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pixeleditor_online_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
