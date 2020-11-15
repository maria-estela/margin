import Margin
import System.Posix.Time
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (forever)
import Text.Printf (printf)
import Text.Read (readEither)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

track items = do
  t1 <- epochTime
  (print.enumerate) items
  l <- getLine
  t2 <- epochTime
  pure (items, t1, t2, l)
 
confirmation :: (Float, String) -> IO ()
confirmation (i, p) = printf "added %f hours for \"%s\"\n" i p

-- throws exceptionsm
marginalise :: ([String], EpochTime, EpochTime, String) -> (Float, String)
marginalise (items, t1, t2, l) =
  let at = (!!)
      e = readEither l :: Either String Int
      p = either (const l) (at items) e
      c = fromIntegral.fromEnum
      secondsPerHour = 3600
      interval = (c t2 - c t1) / secondsPerHour
  in (interval, p)

-- throws exceptions
readItems = do
  args <- getArgs
  contents <- readFile (d args)
  pure (lines contents)
  where d [] = "activities"
        d args = head args

main = do
  i <- readItems
  forever (do
               m <- track i
               addToDefaultFile (marginalise m)
               confirmation (marginalise m)
          )
 
