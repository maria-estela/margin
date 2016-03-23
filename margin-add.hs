import System.IO
import System.Environment( getArgs )
import System.Directory( doesFileExist )
import Text.Read( readMaybe )
import Data.List( uncons, intersperse )
import Data.ByteString.Lazy( readFile, writeFile )
import Data.Maybe( fromMaybe )
import Data.Time( getCurrentTime )
import Data.Aeson( FromJSON, ToJSON, eitherDecode, encode )
import Data.Functor( (<$>) )

import Margin

parseArgs :: [String] -> Maybe (Float, String)
parseArgs args = do
  (valueText, otherArguments) <- uncons args
  value <- (readMaybe valueText :: Maybe Float)
  return (value, concat (intersperse " " otherArguments))

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
  applyToFile "./margin-data.json" (addMargin newMargin)
  return ()

main = do
  args <- getArgs
  case (parseArgs args) of
    Nothing -> putStrLn "usage: margin-add <value> <description>"
    Just valueDesc -> addToDefaultFile valueDesc
