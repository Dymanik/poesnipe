{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Lib where

import Data.Aeson
import Network.HTTP.Conduit
import Data.Aeson.Types
import Data.Maybe
import Control.Applicative
import Data.List
{-import qualified Data.ByteString.Lazy as B-}

data StashList = StashList {
    nextChangeId :: String,
    stashes :: [Stash]
} deriving (Show)

instance FromJSON StashList where
    parseJSON = withObject "StashList" $ \o -> do
        nextChangeId <- o .: "next_change_id"
        stashes <- o .: "stashes"
        return StashList{..}


data Stash = Stash {
    accountName :: Maybe String,
    lastCharacterName :: String,
    stashId :: String,
    stashName :: String,
    stashType :: String,
    items :: [Item],
    public :: Bool
} deriving (Show)

instance FromJSON Stash where
    parseJSON = withObject "Stash" $ \o -> do
        accountName <- o .: "accountName"
        lastCharacterName <- o .: "lastCharacterName"
        stashId <- o .: "id"
        stashName <- o .: "stash"
        stashType <- o .: "stashType"
        items <- o .: "items"
        public <- o .: "public"
        return Stash{..}


data Item = Item {
    {-verified :: Bool,-}
    {-w :: Int,-}
    {-h :: Int,-}
    {-ilvl :: Int,-}
    {-icon :: String,-}
    league :: String,
    {-itemId :: String,-}
    {-sockets :: [Socket],-}
    itemName :: String,
    {-typeLine :: String,-}
    {-identified :: Bool,-}
    {-corrupted :: Bool,-}
    {-lockedToCharacter :: Bool,-}
    note :: Maybe String
    {-properties :: [Property],-}
    {-requirements :: [Requirement],-}
    {-explicitMods :: [String],-}
    {-implicitMods :: [String],-}
    {-enchantMods :: [String],-}
    {-craftedMods :: [String],-}
    {-flavourText :: [String],-}
    {-frameType :: Int,-}
    {-x :: Int,-}
    {-y :: Int,-}
    {-inventoryId :: String,-}
    {-socketedItems :: [Item],-}
    {-additionalProperties :: [Property],-}
    {-secDescrText :: String,-}
    {-descrText :: String,-}
    {-artFilename :: String,-}
    {-duplicated :: Bool,-}
    {-maxStackSize :: Int,-}
    {-nextLevelRequirements :: [Requirement],-}
    {-stackSize :: Int,-}
    {-talismanTier :: Int,-}
    {-utilityMods :: [String],-}
    {-support :: Bool,-}
    {-cosmeticMods :: [String],-}
    {-prophecyDiffText :: String,-}
    {-prophecyText :: String,-}
    {-isRelic :: Bool-}
} deriving (Show)

instance FromJSON Item where
    parseJSON = withObject "Item" $ \o -> do
        name <- o .: "name" 
        typeline <- o .: "typeLine"
        let itemName = name ++ " " ++ typeline
        league <- o .: "league"
        note <- o .:? "note"
        return Item{..}

data Socket = Socket {
    group :: Int,
    attr :: String
}

data Property = Property {
    name :: String,
    values :: [(String,Int)],
    displayMode :: Int,
    proptype :: Int,
    progress :: Int
}

type Requirement = Property 

type Price = String
type League = String

isPrice :: String -> Bool
isPrice s = "~b/o" `isPrefixOf` s || "~price" `isPrefixOf` s

getPrice :: String -> Maybe Price
getPrice s = if ("~b/o" :: String) `isPrefixOf` s || ("~price" :: String) `isPrefixOf` s 
                then Just  s
                else Nothing

setItemPrice :: Item ->  Maybe Price  -> Item
setItemPrice i p = i{note =note i <|> p}

itemsOnSale :: League ->  StashList -> [Item]
itemsOnSale l stash  = filter (\i -> (league i == l) && isJust ( note i )) $ concatMap (\s -> map (\i -> setItemPrice i $ getPrice $ stashName s ) $ items s) $ filter public $ stashes stash


poeNinjaStatsAPIURL :: String
poeNinjaStatsAPIURL  = "http://api.poe.ninja/api/Data/GetStats"

stashAPIURL :: String
stashAPIURL = "http://www.pathofexile.com/api/public-stash-tabs"

parseChangeId :: Value -> Parser String
parseChangeId = withObject "asdf" (.: "nextChangeId")

someFunc :: IO()
someFunc = undefined
