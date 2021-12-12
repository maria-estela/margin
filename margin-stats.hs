import Data.ByteString.Lazy( readFile )
import Margin
import Data.Aeson( eitherDecode )
import Control.Applicative( (<$>) )
import Control.Monad( void )
import System.Environment( getArgs )

analyse :: [Margin] -> (Float, Float)
analyse m = (s, a)
  where s = sum (map value m)
        a = s / fromIntegral (length m)

format :: (Float, Float) -> String
format (s, a) = "sum: " ++ show s ++ ", average: " ++ show a

showStats fileName = do
  putStrLn ("reading file " ++ fileName)
  eitherDecoded <- parseMarginFile fileName
  putStrLn $ either id (format . analyse) eitherDecoded

main = do
  args <- getArgs
  if null args
    then showStats ("./" <> defaultFileName)
    else mapM_ showStats args
