import Data.ByteString.Lazy hiding (map, null)
import Margin
import Data.Aeson( eitherDecode, encode )
import System.Environment( getArgs )
--import Control.Applicative( (<$>) )
import Control.Monad( sequence )
import Data.Either( partitionEithers )
import System.IO( stderr, hPutStrLn )

maybeDecodeAll :: [ByteString] -> Either [String] [[Margin]]
maybeDecodeAll contents
  | null l = Right r
  | otherwise = Left l
  where (l, r) = partitionEithers (map eitherDecode contents)

main = do
  args <- getArgs
  contents <- sequence (map Data.ByteString.Lazy.readFile args)
  case maybeDecodeAll contents of
    Left errors -> do
      sequence (map (hPutStrLn stderr) errors)
      return ()
    Right decoded -> (Data.ByteString.Lazy.putStr . encode . mconcat) decoded
