{-# LANGUAGE DeriveGeneric #-}
module Margin where

import Data.Semigroup
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Time.Clock (UTCTime)
import Data.Either (rights, lefts)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (readFile)

defaultFileName = "margin-data.json"

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Margin
instance ToJSON Margin

parseMarginFile :: String -> IO (Either String [Margin])
parseMarginFile path = do
  contents <- Data.ByteString.Lazy.readFile path
  return (eitherDecode contents)

getAllMargins :: [String] -> IO [Margin]
getAllMargins paths = do
  eitherDecoded <- sequence (map parseMarginFile paths)
  sequence ((map print . lefts) eitherDecoded)
  return ((concat . rights) eitherDecoded)

-- helper for command line scripts. Gets a list of paths and a
-- function to execute on them if they are all correctly parsed as
-- margins
onAllMargins paths fun = do
  margins <- getAllMargins paths
  (putStr . fun) margins
