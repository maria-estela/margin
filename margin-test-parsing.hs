import Margin (parseMarginFile)
import System.Environment (getArgs)

respond = either id (const "parsing successful")

main = do
  [path] <- getArgs
  result <- parseMarginFile path
  print (respond result)
