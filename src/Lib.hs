{-# LANGUAGE RecordWildCards,OverloadedStrings #-}
module Lib where

import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Data.Aeson.Types
import Data.Maybe
import Control.Applicative
import Control.Monad.Loops
import Control.Monad
import Data.List
import Data.List.Split
import Price
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as B

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
    price :: Maybe Price
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
        let itemName = last (splitOn ">" name) ++ " " ++ last (splitOn ">" typeline)
        league <- o .: "league"
        note <- o .:? "note"
        let price = parsePrice =<< note
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

type League = String

isPrice :: String -> Bool
isPrice s = "~b/o" `isPrefixOf` s || "~price" `isPrefixOf` s

setItemPrice :: Item ->  Maybe Price  -> Item
setItemPrice i p = i{price =price i <|> p}

itemsOnSale :: StashList -> [Item]
itemsOnSale stash  = filter (isJust . price ) $ concatMap (\s ->
    map (\i -> setItemPrice i $ parsePrice $ stashName s ) $ items s) $ filter public $ stashes stash


poeNinjaStatsAPIURL :: String
poeNinjaStatsAPIURL  = "http://api.poe.ninja/api/Data/GetStats"

stashAPIURL :: String
stashAPIURL = "http://www.pathofexile.com/api/public-stash-tabs?id="

parseChangeId :: Value -> Parser String
parseChangeId = withObject "ChangeId" (.: "nextChangeId")

getLatestChangeId :: IO (Maybe String)
getLatestChangeId = getChange <$> simpleHttp poeNinjaStatsAPIURL
    where
        getChange bs = parseMaybe parseChangeId =<< decode bs

getStashList :: String -> IO (Maybe StashList)
getStashList [] =do
    chid <- fromJust <$> getLatestChangeId
    parseStash <$> simpleHttp (stashAPIURL++chid)
    where
        parseStash bs = decode bs :: Maybe StashList
getStashList chid = parseStash <$> simpleHttp (stashAPIURL++chid)
    where
        parseStash bs = decode bs :: Maybe StashList

infiniteStash :: IO [Maybe StashList]
infiniteStash =   (:) <$> getStashList ""  <*> unsafeInterleaveIO (next infiniteStash) 
        where
            next :: IO [Maybe StashList] -> IO [Maybe StashList]
            next l = do
                (x:_) <- l
                (:) <$> getStashList (nextChangeId$ fromJust x) <*> unsafeInterleaveIO (next (fmap tail l))

{-infiniteStash :: [IO (Maybe StashList)]-}
{-infiniteStash = getStashList "" : next infiniteStash-}
    {-where-}
        {-next :: [IO (Maybe StashList)] -> [IO (Maybe StashList)]-}
        {-next (x:xs) = (fmap (fmap nextChangeId) x >>= getStashList . fromJust) : next xs-}

someFunc :: IO()
someFunc = undefined
