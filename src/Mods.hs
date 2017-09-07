module Mods where

import Control.Monad (void)
import Data.Void
import Data.Char(toLower)
import Data.List


data Mod = LiteralMod {modName:: String}
         deriving (
             Show,
             Eq
         )

         
parseMod :: String -> Mod
parseMod = LiteralMod  .  map toLower 





