{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
import Control.Applicative( (<$>) )
import Control.Monad (void, join)
import Data.Aeson( eitherDecode )
import Data.Bool (bool)
import Data.ByteString.Lazy( readFile )
import Data.List (intercalate, sortOn)
import Data.Map hiding (map, splitAt)
import Margin (Margin, l)
import Options.Applicative

import qualified Margin

data Accum a = A {
  count :: Int,
  real :: Float,
  overlapping :: Float,
  attribution :: a
  }

instance Semigroup s => Semigroup (Accum s) where
  (<>) (A c1 r1 o1 a1) (A c2 r2 o2 a2) = A (c1+c2) (r1+r2) (o1+o2) (a1<>a2)

instance Monoid m => Monoid (Accum m) where
  mempty = A 0 0 0 mempty

instance Functor Accum where
  fmap f a = a { attribution = f (attribution a) }

newtype Options = Options { targets :: [String] }

options :: Parser Options
options = Options <$> many (strOption (short 'f'))

format :: Accum (Map String Float) -> String
format A {count, real, overlapping, attribution} =
  let
    e = toAscList attribution
    fe (label, float) =
      let s f
            | f < 0.1 = "<0.1 " -- exponential notation starts from e^-2
            | otherwise = uncurry etc . splitAt 4 . show $ f
      in unwords [s (float/real), s float, label]
    etc :: [Char] -> [Char] -> [Char]
    etc c [] = c <> " "
    etc c _  = c <> "_"
    footer = intercalate ", " [
      "real" `l` real,
      "average" `l` (real / fromIntegral count),
      "overlap" `l` (overlapping/real - 1) <> "%"]
  in unlines $ (fmap fe . sortOn snd $ e) <> ["\n", footer]

a :: Margin -> Accum [(String, Float)]
a m = A { count=1, real=v, overlapping=v*l, attribution=attributed }
  where
    v = Margin.value m
    l = fromIntegral . length $ attributed
    b [] = ["<empty>"]
    b w = w
    attributed :: [(String, Float)]
    attributed = fmap (, v) . b . words . Margin.description $ m

s =
  fmap (fromListWith (+)) .
  -- ^ Could be done at first pass for performance
  foldMap a

main = do
  Options{..} <- execParser (info options fullDesc)
  putStrLn . intercalate "\n" =<< Margin.onAllMargins' (format . s) targets
