{-# LANGUAGE DeriveGeneric #-}
module Margin where

import Data.Semigroup
import Data.Aeson( FromJSON, ToJSON )
import GHC.Generics
import Data.Time.Clock( UTCTime )
import Data.Either( isRight, rights)
import Data.Aeson( eitherDecode )
import Data.ByteString.Lazy.Char8( pack )

defaultFileName = "margin-data.json"

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Margin
instance ToJSON Margin

-- code below is used by `onAllMargins`

areRights :: [Either a b] -> [Bool]
areRights = map isRight

allRights :: [Either a b] -> Bool
allRights = and . areRights

readMarginFiles :: [String] -> IO [String]
readMarginFiles names = sequence $ map readFile names

getAllMargins :: [String] -> IO [Margin]
getAllMargins paths = do
  contents <- readMarginFiles paths
  let parsed = map (eitherDecode . pack) contents
    in if (allRights parsed)
       then
         (return . concat . rights) parsed
       else do
         print "parsing error"
         return []

-- helper for command line scripts. Gets a list of paths and a
-- function to execute on them if they are all correctly parsed as
-- margins
onAllMargins paths fun = do
  margins <- getAllMargins paths
  (putStr . fun) margins
