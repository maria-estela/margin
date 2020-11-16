import Margin
import System.Posix.Time
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (forever, void, join)
import Text.Printf (printf)
import Text.Read (readEither)
import Data.List (partition)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

track items = do
  t1 <- epochTime
  l <- getLine
  t2 <- epochTime
  pure (items, t1, t2, l)
 
confirmation :: String -> (Float, String) -> IO ()
confirmation action (i, p)
  | i >= 1    = printf "%s %f hours for \"%s\"\n"   action i p
  | otherwise = printf "%s %f minutes for \"%s\"\n" action m p
  where m = i*60

-- throws exceptionsm
toMargin :: ([String], EpochTime, EpochTime, String) -> (Float, String)
toMargin (items, t1, t2, l) =
  let at = (!!)
      e = readEither l :: Either String Int
      p = either (const l) (at items) e
      c = fromIntegral.fromEnum
      secondsPerHour = 3600
      interval = (c t2 - c t1) / secondsPerHour
  in (interval, p)

readConf = do
  args <- getArgs
  contents <- readFile (def args)
  pure (lines contents, complementar args)
    where def args
            | null (fils args) = if (complementar args)
                                  then "complementar-activities"
                                  else "activities"
            | otherwise        = head (fils args)
          optionsAndFiles :: [String] -> ([String], [String])
          optionsAndFiles = partition (=="-c")
          opts = fst . optionsAndFiles
          fils = snd . optionsAndFiles
          complementar :: [String] -> Bool
          complementar args = not (null (opts args)) 

step :: ([String], Bool) -> Bool -> IO Bool
step (items, complementar) logging = do
  case (complementar, logging) of
    (False, True) -> (do
              (print.enumerate) items
              m <- track items
              addToDefaultFile (toMargin m)
              confirmation "added" (toMargin m))
    (False, False) -> (do
              putStrLn "enter to keep logging"
              m <- track []
              confirmation "elapsed" (toMargin m))
    (True, False) -> (do
              (print.enumerate) items
              m <- track items
              confirmation "elapsed" (toMargin m))
    (True, True) -> (do
              putStrLn "enter to stop logging"
              m <- track []
              addToDefaultFile (toMargin m)
              confirmation "added" (toMargin m))
  return (not logging) -- currently ignored

main = do
  conf <- readConf
  forever (void (sequence (map (step conf) (join (repeat [True, False])))))
 
