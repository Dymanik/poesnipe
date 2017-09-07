module Main where

import Lib
import Data.Maybe
import Data.List
import Mods
import System.Environment

main :: IO ()
main = do
    (item:mods) <- getArgs 
    let filtermod = map (\m im -> m `isInfixOf` modName im) mods
    putStrLn =<< fmap ( unlines . map show . 
                      filter (\i ->
                              (league i == "Harbinger") &&
                              item `isInfixOf` itemName i &&
                              all (isJust . flip itemFilterMod i) filtermod) 
                                . concatMap itemsOnSale . catMaybes ) infiniteStash

