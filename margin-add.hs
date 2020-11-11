import Data.List( uncons, intersperse )
import System.IO
import System.Environment( getArgs )
import Text.Read( readMaybe )
import Margin

parseArgs :: [String] -> Maybe (Float, String)
parseArgs args = do
  (valueText, otherArguments) <- uncons args
  value <- (readMaybe valueText :: Maybe Float)
  return (value, concat (intersperse " " otherArguments))

main = do
  args <- getArgs
  case (parseArgs args) of
    Nothing -> putStrLn "usage: margin-add <value> <description>"
    Just valueDesc -> addToDefaultFile valueDesc
