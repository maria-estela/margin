import Data.ByteString.Lazy hiding (map, null, concat)
import Margin
import System.Environment( getArgs )
import Data.Either( partitionEithers )
import System.IO( stderr, hPutStrLn )

p :: [Either a b] -> Either [a] [b]
p eithers
  | null l = Right r
  | otherwise = Left l
  where (l, r) = partitionEithers eithers

main = do
  args <- getArgs
  eitherDecoded <- mapM parseMarginFile args
  case p eitherDecoded of
    Left errors ->
      mapM_ (hPutStrLn stderr) errors
    Right margins ->
      (Data.ByteString.Lazy.putStr . formatMarginContents . concat) margins
