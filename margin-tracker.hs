import Margin
import System.Posix.Time
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (forever, void, join)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Safe (atMay)
import Data.List (partition)
import Data.Set (Set (..), fromList, singleton, toList, elemAt, union)
import Control.Monad (when)

enumerate :: Set a -> [(Int, a)]
enumerate = zip [0..] . toList

data Conf = Conf { confItems::(Set String), confComplementar::Bool }
type Description = String

track items = do
  t1 <- epochTime
  l <- getLine
  t2 <- epochTime
  return (toMargin items (t1, t2, l))
 
toMargin :: Set String -> (EpochTime, EpochTime, String) -> (Float, String)
toMargin items (t1, t2, l) =
  let m :: Maybe String
      m = do
        i <- readMaybe l
        atMay (toList items) i
      p :: String
      p = maybe l id m
      c = fromIntegral.fromEnum
      secondsPerHour = 3600
      interval = (c t2 - c t1) / secondsPerHour
  in (interval, p)

printInterval :: Float -> IO ()
printInterval h
  | h >= 1    = printf "%f hours\n"   h
  | otherwise = printf "%f minutes\n" m
  where m = h*60

readConf = do
  args <- getArgs
  contents <- readFile (def args)
  pure (Conf (items contents) (complementar args))
    where def args
            | null (fils args) = if (complementar args)
                                  then "interruptions"
                                  else "activities"
            | otherwise        = head (fils args)
          optionsAndFiles :: [String] -> ([String], [String])
          optionsAndFiles = partition (=="-c")
          opts = fst . optionsAndFiles
          fils = snd . optionsAndFiles
          complementar :: [String] -> Bool
          complementar args = not (null (opts args))
          items = fromList . lines

step :: Conf -> Bool -> IO Bool
step conf@(Conf items complementar) logging = do
  putStrLn (if active
    then show (enumerate items)
    else if complementar
         then "enter to select an interruption"
         else "enter to select an activity")
  (t, d) <- track items
  when logging (do
    addToDefaultFile (t,d)
    putStrLn ("added "++d++" to default margin file"))
  printInterval t
  let newConf = if active then addItem d else conf
    in step newConf (not logging)
  where active = logging /= complementar
        addItem description = conf {
          confItems = union (singleton description) items
          }

main = do
  c <- readConf
  forever (step c True)
 
