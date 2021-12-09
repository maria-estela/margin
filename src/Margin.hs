{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Margin where

import Control.Monad (when)
import Data.Aeson(eitherDecode, FromJSON, ToJSON, eitherDecode, encode )
import Data.ByteString.Lazy( readFile, writeFile )
import Data.Either( rights, lefts, isRight)
import Data.Functor( (<$>) )
import Data.Maybe( fromMaybe )
import Data.Semigroup
import Data.Time.Clock (UTCTime)
import Data.Time( getCurrentTime )
import GHC.Generics
import System.Directory( doesFileExist )
import Data.Csv ((.:), (.=))

import qualified Data.Csv as Csv
import qualified Data.Vector as Vector
import qualified Data.Time.ISO8601 as ISO8601
import qualified Data.ByteString.Char8 as Char8

defaultFileName = "margin-data.json"

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Margin
instance ToJSON Margin

instance Csv.FromField UTCTime where
  parseField packed =
    let t = Char8.unpack packed
    in case ISO8601.parseISO8601 t of
      Just time -> pure time
      Nothing -> fail (t <> " is not an ISO8601 time")

instance Csv.ToField UTCTime where
  toField = Char8.pack . ISO8601.formatISO8601Millis

instance Csv.FromNamedRecord Margin where
  parseNamedRecord r = Margin
    <$> r .: "value"
    <*> r .: "description"
    <*> r .: "time"

instance Csv.ToNamedRecord Margin where
  toNamedRecord m = Csv.namedRecord [
    "value" .= value m,
    "description" .= description m,
    "time" .= time m]

instance Csv.DefaultOrdered Margin where
  headerOrder _ = Csv.header ["time", "value", "description"]

parseMarginFile :: FilePath -> IO (Either String [Margin])
parseMarginFile path = do
  contents <- Data.ByteString.Lazy.readFile path
  case Csv.decodeByName contents of
    Right (_, margins) -> pure $ Right $ Vector.toList margins
    Left err -> do
      putStrLn $ err <> ", trying old JSON format"
      let eitherDecoded = eitherDecode contents
      when (isRight eitherDecoded) (putStrLn "this JSON margin format is deprecated")
      pure eitherDecoded

serialiseMarginFile :: [Margin] -> FilePath -> IO ()
serialiseMarginFile margins path =
  Data.ByteString.Lazy.writeFile path (Csv.encodeDefaultOrderedByName margins)

getAllMargins :: [String] -> IO [Margin]
getAllMargins paths = do
  eitherDecoded <- mapM parseMarginFile paths
  mapM print (lefts eitherDecoded)
  pure ((concat . rights) eitherDecoded)

-- helper for command line scripts. Gets a list of paths and a
-- function to execute on them if they are all correctly parsed as
-- margins
onAllMargins paths fun = do
  margins <- getAllMargins paths
  (putStr . fun) margins

marginNow :: Float -> String -> IO Margin
marginNow value description = Margin value description <$> getCurrentTime

applyToMargin
  :: FilePath
  -> (Maybe [Margin] -> [Margin])
  -> IO (Either String [Margin])
applyToMargin path f = do
  exists <- doesFileExist path
  if exists
    then
    do
      eitherParsed <- parseMarginFile path
      case eitherParsed of
        Left error -> pure (Left error)
        Right decoded -> do
          serialiseMarginFile processed path
          pure (Right processed)
            where processed = f (Just decoded)
    else
    do
      serialiseMarginFile processed path
      pure (Right processed)
        where processed = f Nothing

addMargin :: Margin -> Maybe [Margin] -> [Margin]
addMargin newMargin maybeMargins = margins ++ [newMargin]
  where margins = fromMaybe [] maybeMargins

addToFile fileName (value, description) = do
  newMargin <- marginNow value description
  applyToMargin fileName (addMargin newMargin)
  pure ()

addToDefaultFile = addToFile ("./" ++ defaultFileName)

addToMaybeFile :: Maybe FilePath -> (Float, String) -> IO ()
addToMaybeFile Nothing = addToDefaultFile
addToMaybeFile (Just path) = addToFile path

parseMaybeFile = parseMarginFile . fromMaybe defaultFileName
