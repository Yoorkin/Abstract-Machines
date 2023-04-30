{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_machines (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\bin"
libdir     = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\lib\\x86_64-windows-ghc-9.2.7\\machines-0.1.0.0-EdluixrYoF1Hjrrtt1bsQ2-machines"
dynlibdir  = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\lib\\x86_64-windows-ghc-9.2.7"
datadir    = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\share\\x86_64-windows-ghc-9.2.7\\machines-0.1.0.0"
libexecdir = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\libexec\\x86_64-windows-ghc-9.2.7\\machines-0.1.0.0"
sysconfdir = "E:\\sources\\machines\\.stack-work\\install\\bb634ddb\\etc"

getBinDir     = catchIO (getEnv "machines_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "machines_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "machines_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "machines_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "machines_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "machines_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
