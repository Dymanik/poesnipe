module Price where

import Text.Read
import Data.List
import Data.List.Split
import Debug.Trace 

data Currency = Exalted 
              | Chaos 
              | Chromatic
              | Alchemy
              | Alteration
              | Jeweller
              | Chance
              | Chisel
              | Fusing
              | Scouring
              | Blessed
              | Regal
              | Gemcutter
              | Divine
              | Mirror
              | Perandus
              | Silver
              deriving (
                  Show,
                  Eq
              )

data  Price = SimplePrice Float Currency
            | BulkPrice Float Float Currency 
            deriving(
                Show
            )

parsePrice :: String -> Maybe Price
parsePrice = go . words 
    where
        go (x:xs)
            | x =="~b/o" =  am xs
            | x =="~price" = am xs
            | otherwise = Nothing
        go _ = Nothing
        am (x:y:xs) = case splitOn "/" x of
                           (n:d:_) ->  BulkPrice <$> readMaybe n <*> readMaybe d <*> curr y
                           (n:xs) ->  SimplePrice <$> readMaybe n <*> curr y
                           _ -> Nothing
        am _ = Nothing
        curr c
            | "exa" `isPrefixOf` c = Just Exalted
            | "chaos" `isPrefixOf` c = Just Chaos
            | "chrom" `isPrefixOf` c = Just Chromatic
            | "alch" `isPrefixOf` c = Just Alchemy
            | "alt" `isPrefixOf` c = Just Alteration
            | "jew" `isPrefixOf` c = Just Jeweller
            | "chance" `isPrefixOf` c = Just Chance
            | "chisel" `isPrefixOf` c = Just Chisel
            | "fus" `isPrefixOf` c = Just Fusing
            | "scour" `isPrefixOf` c = Just Scouring
            | "bless" `isPrefixOf` c = Just Blessed
            | "regal" `isPrefixOf` c = Just Regal
            | "gcp" `isPrefixOf` c = Just Gemcutter
            | "divine" `isPrefixOf` c = Just Divine
            | "mirror" `isPrefixOf` c = Just Mirror
            | "perandus" `isPrefixOf` c = Just Perandus
            | "silver" `isPrefixOf` c = Just Silver
            | otherwise = Nothing
