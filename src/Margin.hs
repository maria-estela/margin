{-# LANGUAGE DeriveGeneric #-}
module Margin where

import Data.Aeson (eitherDecode)
import Data.Aeson( FromJSON, ToJSON, eitherDecode, encode )
import Data.ByteString.Lazy( readFile, writeFile )
import Data.Either (rights, lefts)
import Data.Functor( (<$>) )
import Data.Maybe( fromMaybe )
import Data.Semigroup
import Data.Time.Clock (UTCTime)
import Data.Time( getCurrentTime )
import GHC.Generics
import System.Directory( doesFileExist )

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

marginNow :: Float -> String -> IO Margin
marginNow value description = (Margin value description) <$> getCurrentTime

applyToFile :: (FromJSON a, ToJSON a) => FilePath -> (Maybe a -> a) -> IO (Either String a)
applyToFile path f = do
  exists <- doesFileExist path
  if exists
    then
    do
      string <- Data.ByteString.Lazy.readFile path
      case (eitherDecode string) of
        Left error -> return (Left error)
        Right decoded -> do
          Data.ByteString.Lazy.writeFile path (encode processed)
          return (Right processed)
            where processed = f (Just decoded)
    else
    do
      Data.ByteString.Lazy.writeFile path (encode processed)
      return (Right processed)
        where processed = f Nothing

addMargin :: Margin -> Maybe [Margin] -> [Margin]
addMargin newMargin maybeMargins = margins ++ [newMargin]
  where margins = fromMaybe [] maybeMargins

addToDefaultFile (value, description) = do
  newMargin <- marginNow value description
  applyToFile ("./" ++ defaultFileName) (addMargin newMargin)
  return ()
