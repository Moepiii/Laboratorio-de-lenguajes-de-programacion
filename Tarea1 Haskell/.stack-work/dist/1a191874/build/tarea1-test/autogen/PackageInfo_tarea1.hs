{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_tarea1 (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "tarea1"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Tarea 1 - Haskell SOS"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
