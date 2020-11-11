import Margin
import System.Posix.Time
import System.Environment (getArgs)
import Control.Monad (forever)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

-- throws exceptions
marginalise (items, t1, t2, l) =
  let i = read l :: Int
      p = items!!i
      c = fromIntegral.fromEnum
      secondsPerHour = 3600
      interval = (c t2 - c t1) / secondsPerHour
  in (interval, p)

track items = do
  t1 <- epochTime
  (print.enumerate) items
  l <- getLine
  t2 <- epochTime
  pure (items, t1, t2, l)
 
readItems = do
  [fileName] <- getArgs
  contents <- readFile fileName
  pure (lines contents)

main = do
  i <- readItems
  forever (do
               m <- track i
               addToDefaultFile (marginalise m)
          )
 
