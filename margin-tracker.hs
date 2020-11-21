{-

Enter submits, Ctrl-D (or Ctrl-C) cancels

-}
import Margin
import System.Posix.Time
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (forever, void, join, when)
import System.IO.Error
import Control.Exception (try)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Safe (atMay)
import Data.List (partition)
import Data.Set (Set (..), fromList, singleton, empty, toList, elemAt, union)

enumerate :: Set a -> [(Int, a)]
enumerate = zip [0..] . toList

data State = State {
  stateItems        :: Set String,
  stateComplementar :: Bool,
  stateLogging      :: Bool
  }

data Message = Enumerate (Set String)
             | Enter Bool
             | Added String
             | Elapsed Float
             | Started String
             | CancelTracking
             | CancelStep

showMessage :: Message -> String
showMessage (Enumerate items) = show (enumerate items)
showMessage (Enter complementar)
  | complementar = "enter to select an interruption"
  | otherwise    = "enter to select an activity"
showMessage (Added description) = "added "++description++" to margin file"
showMessage (Elapsed h)
  | h >= 1    = (show h)++" hours"
  | otherwise = (show m)++" minutes"
  where m = h*60
showMessage (Started activity) = "started logging "++activity
showMessage CancelTracking = "tracking discarded"
showMessage CancelStep     = "quitting..."

printMessage :: Message -> IO ()
printMessage = putStrLn . showMessage

active :: State -> Bool
active s = stateLogging s /= stateComplementar s

step :: State -> IO (Maybe State)
step state@(State items complementar logging) =
  let track :: String -> IO (Either IOError (EpochTime, EpochTime))
      track activity = do
        printMessage (Started activity)
        t1 <- epochTime
        eitherEnter <- try getLine
        t2 <- epochTime
        return (fmap (const (t1, t2)) eitherEnter)
      selected :: String -> Maybe String
      selected userLine = readMaybe userLine >>= atMay (toList items)
      floatFromInterval :: (EpochTime, EpochTime) -> Float
      floatFromInterval (t1, t2) =
        let convert = fromIntegral . fromEnum
            secondsPerHour = 3600
        in (convert t2 - convert t1) / secondsPerHour
      store description hours = do
        addToDefaultFile (hours, description)
        printMessage (Added description)
      onInterval :: String -> (EpochTime, EpochTime) -> IO ()
      onInterval description interval =
        let hours = floatFromInterval interval
        in do
          when (stateLogging state) (store description hours)
          printMessage (Elapsed hours)
      onError :: IOError -> IO (Maybe State)
      onError _ = do
        printMessage CancelStep
        return Nothing
      onTrackingError :: IOError -> IO ()
      onTrackingError _ = do
        printMessage CancelTracking
      onUser :: String -> IO (Maybe State)
      onUser userLine = 
        let description = maybe userLine id (selected userLine)
            addItems newItems = union newItems (stateItems state)
            newItems = if active state then singleton description else empty
            newState = state {
              stateItems = addItems (newItems),
              stateLogging = not (stateLogging state) }
        in do
          eitherInterval <- track description
          either onTrackingError (onInterval description) eitherInterval
          step newState
  in if (active state)
     then (do
               printMessage (Enumerate items)
               eitherUser <- try getLine
               either onError onUser eitherUser)
     else (do
               printMessage (Enter complementar)
               eitherInterval <- track "complementar"
               either onTrackingError (onInterval "complementar") eitherInterval
               step (state { stateLogging = not logging }))

parseArgs :: [String] -> ([FilePath], Bool)
parseArgs args = (paths, complementar)
  where
    paths
      | null fils = if complementar
                    then ["interruptions"]
                    else ["activities"]
      | otherwise = fils
    opts, fils :: [String]
    (opts, fils) = partition (=="-c") args
    complementar = not (null opts)

makeItems :: Either IOError [String] -> Set String
makeItems = fromList . either (const []) parseContents 
  where parseContents cont = join (map lines cont)

loop :: Monad m => (a -> m (Maybe a)) -> a -> m ()
loop f i = do
  i' <- f i
  maybe (return ()) (loop f) i'

main = do
  args <- getArgs
  let (paths, complementar) = parseArgs args
      readItems = sequence (map readFile paths)
    in (do
      contents <- try readItems
      let initial = State (makeItems contents) complementar True
        in loop step initial)
