{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Control.Applicative
-- import Data.Text
import System.Locale
import Data.Time
import Data.Time.Format
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as H

zeroMargin :: Day -> Margin
zeroMargin day = Margin {amount=0, time=(show day), description="zero"}

allDays :: (Day, Day) -> [Day]
allDays (d1, d2)
  | d1 >= d2  = [d2]
  | otherwise = d1:(allDays ((addDays 1 d1), d2))

boundaries :: [Margin] -> (Day, Day)
boundaries margins = (minimum days, maximum days)
  where days = map (read . time) margins

marginDays :: [Margin] -> [Day]
marginDays = allDays . boundaries

fillers :: [Margin] -> [Margin]
fillers margins = map zeroMargin days
  where days = marginDays margins

parseMyTime :: String -> Day
parseMyTime = readTime defaultTimeLocale "%a %b %d %Y %H:%M:%S GMT%z (%Z)"

data Margin = Margin {
  amount :: Int,
  description :: String,
  time :: String
  } deriving Show

instance FromJSON Margin where
  parseJSON (Object v) = Margin <$>
                         v .: "n" <*>
                         v .: "d" <*>
                         v .: "t"
  -- parseJSON _ = Nothing

sum :: Margin -> Margin -> Margin
sum m1 m2 = Margin {
    amount = (amount m1) + (amount m2),
    description = (description m1) ++ ", " ++ (description m2),
    time = (time m2)
  }

maybeSum :: Margin -> Maybe Margin -> Margin
maybeSum m1 Nothing = m1
maybeSum m1 (Just m2) = Main.sum m1 m2

toDay :: String -> String
toDay s = show $ parseMyTime s

aggregate :: [Margin] -> H.HashMap String Margin
aggregate []       = H.empty
aggregate (m:rest) = H.insert (time s) s hash
  where hash = aggregate rest
        s = maybeSum m (H.lookup (time m) hash)

-- i omit the description because of the commas in there
toRow :: Margin -> String
toRow m = (show (time m)) ++ "," ++ (show (amount m))

toRows :: H.HashMap String Margin -> [String]
toRows hash = map toRow $ H.elems hash

fill hash [] = hash
fill hash (margin:rest)
  | H.member day hash = fill hash rest
  | otherwise       = fill (H.insert day margin hash) rest
  where day = time margin

fillDays :: H.HashMap String Margin -> H.HashMap String Margin
fillDays hash  = fill hash (fillers (H.elems hash))

toCSV :: H.HashMap String Margin -> String
toCSV hash = "DATE,VALUE\n" ++ joined
  where joined = foldr1 (\x y -> x ++ "\n" ++ y) rows
        rows = toRows $ fillDays hash

convertDay m = Margin {amount=amount m, time=(toDay (time m)), description=""}

toDays = map convertDay

maybeConvert :: Maybe [Margin] -> String
maybeConvert (Just margins) = toCSV $ aggregate $ toDays margins
maybeConvert Nothing = "parse error"

convert :: L.ByteString -> String
convert = maybeConvert . decode

-- let d = (decode <$> f) :: IO (Maybe [Spending])

main = do
  contents <- L.readFile "file.json"
  writeFile "file.csv" $ convert contents
