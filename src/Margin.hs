{-# LANGUAGE DeriveGeneric #-}
module Margin where

import Data.Semigroup
import Data.Aeson( FromJSON, ToJSON )
import GHC.Generics
import Data.Time.Clock( UTCTime )

defaultFileName = "margin-data.json"

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Generic)

instance FromJSON Margin
instance ToJSON Margin

