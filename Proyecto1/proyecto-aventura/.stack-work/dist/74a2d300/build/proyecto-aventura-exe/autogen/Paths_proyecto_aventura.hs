{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_proyecto_aventura (
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
bindir     = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\bin"
libdir     = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\lib\\x86_64-windows-ghc-9.4.7\\proyecto-aventura-0.1.0.0-AelRadAeczgJv5cKR24yJW-proyecto-aventura-exe"
dynlibdir  = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\lib\\x86_64-windows-ghc-9.4.7"
datadir    = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\share\\x86_64-windows-ghc-9.4.7\\proyecto-aventura-0.1.0.0"
libexecdir = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\libexec\\x86_64-windows-ghc-9.4.7\\proyecto-aventura-0.1.0.0"
sysconfdir = "C:\\Users\\TT\\Desktop\\Lab de lenguajes\\Proyecto1\\proyecto-aventura\\.stack-work\\install\\8affafeb\\etc"

getBinDir     = catchIO (getEnv "proyecto_aventura_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "proyecto_aventura_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "proyecto_aventura_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "proyecto_aventura_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proyecto_aventura_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proyecto_aventura_sysconfdir") (\_ -> return sysconfdir)




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
