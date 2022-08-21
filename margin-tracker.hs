{-# LANGUAGE RecordWildCards #-}

import Margin
import System.Posix.Time
import System.Posix.Types
import System.Environment (getArgs)
import Control.Monad (join, sequence, when)
import System.IO.Error ()
import Control.Exception (try)
import Text.Read (readMaybe)
import Safe (atMay)
import Data.List (partition, intercalate, sortOn, nub)
import Data.Maybe (fromMaybe)
import Data.Set (
  Set (..),
  fromList,
  singleton,
  toList,
  difference,
  union)
import Options.Applicative
import Data.Semigroup ((<>))

selectionList :: Set String -> [String]
selectionList = sortOn length . toList

enumerate :: Set String -> [(Int, String)]
enumerate = zip [0..] . selectionList

data StepState = Logging | Prompt

data State = State {
  stateItems   :: Set String,
  stateContext :: [String],
  stateStep :: StepState
  }

data Message = Enumerate State
             | Paused
             | Added String
             | Elapsed Float
             | Started String
             | CancelTracking
             | QuitMessage

showMessage :: Message -> String
showMessage (Enumerate state) = show (enumerate (stateItems state))
                                ++ "\n"
                                ++ intercalate " > " (reverse (stateContext state))
showMessage Paused = "paused. enter to select an activity"
showMessage (Added description) = "added "++description++" to margin file"
showMessage (Elapsed h)
  | h >= 1    = show h <> " hours"
  | otherwise = show m <> " minutes"
  where m = h*60
showMessage (Started activity) = "started logging "++activity
showMessage CancelTracking = "tracking discarded"
showMessage QuitMessage     = "quitting context"

printMessage :: Message -> IO ()
printMessage = putStrLn . showMessage

data Command = Quit | Deepen String | Continue deriving Eq

interpretCommand :: Either IOError String -> Command
interpretCommand (Left _)   = Quit
interpretCommand (Right "") = Continue
interpretCommand (Right s)  = Deepen s

-- @updateState@ is used only on @Prompt@ states
updateState :: Command -> State -> State
updateState Quit state@(State items context Prompt) =
  state {
    stateContext = tail context,
    stateItems = items `union` fromList (words (head context))
    }
updateState Continue state = state { stateStep = Logging }
updateState (Deepen userLine) state@(State items context Prompt) =
  let
    selected :: Maybe String
    selected = readMaybe userLine >>= atMay (selectionList items)
    selection = fromMaybe userLine selected
  in state {
    stateContext = selection:context,
    stateItems = difference items (fromList (words selection))
    }

type MarginData = (Float, String)

step :: State -> IO [MarginData]

step state@(State _ context Prompt) = do
  printMessage (Enumerate state)
  userLine <- try getLine
  let command = interpretCommand userLine
  if command == Quit && null context
    then return []
    else step (updateState command state)

step state@(State _ context Logging) = do
  eitherInterval <- track
  either onTrackingError onInterval eitherInterval
  where nextStep = step state { stateStep = Prompt }
        description = unwords (reverse context)
        track :: IO (Either IOError (EpochTime, EpochTime))
        track = do
          printMessage (Started description)
          t1 <- epochTime
          eitherEnter <- try getLine
          t2 <- epochTime
          return (fmap (const (t1, t2)) eitherEnter)
        onInterval :: (EpochTime, EpochTime) -> IO [MarginData]
        onInterval interval =
          let hours = floatFromInterval interval
          in do
            printMessage (Elapsed hours)
            nextData <- nextStep
            pure ((hours, description) : nextData)
        onTrackingError :: IOError -> IO [MarginData]
        onTrackingError _ = do
          printMessage CancelTracking
          nextStep
        floatFromInterval :: (EpochTime, EpochTime) -> Float
        floatFromInterval (t1, t2) =
          let convert = fromIntegral . fromEnum
              secondsPerHour = 3600
          in convert (t2 - t1) / secondsPerHour

defActivities :: [FilePath] -> [FilePath]
defActivities [] = ["margin-tracker-activities"]
defActivities o  = o

makeItems
  :: Either IOError [String]
  -> Set String
  -> Set String
makeItems eitherContents = union contentSet
  where
    contentSet = fromList $ either (const []) parseContents eitherContents
    parseContents cont = lines =<< cont

data Options = Options {
  activityFiles :: [String],
  marginFile :: Maybe String,
  marginDescriptionLimit :: Maybe Int,
  verbose :: Bool
  }

options :: Parser Options
options = Options
          <$> many (strOption (long "activities"))
          <*> optional (strOption (long "margin-data"))
          <*> optional (option auto (long "margin-description-limit"))
          <*> switch (long "verbose")

main :: IO ()
main = do
  Options{..} <- execParser (info options fullDesc)
  eitherEitherMargin <- try $ parseMaybeFile marginFile
  eitherMargin <- either explainParsing pure eitherEitherMargin
  let parsed = parseMargins $ either (const []) id eitherMargin
      taken = take (fromMaybe 10 marginDescriptionLimit) parsed
  when verbose (showMargins parsed taken)
  let paths = defActivities activityFiles
  eitherContents <- try $ mapM readFile paths
  let activities = makeItems eitherContents $ fromList taken
  trackingData <- step (State activities [] Prompt)
  mapM_ (addToMaybeFile marginFile) trackingData

  where
    explainParsing error = do
      print (error :: IOError)
      putStrLn "no activities will be taken by the file"
      pure $ Right []
    parseMargins margin =
      sortOn length $ nub ((words . description) =<< margin)
    showMargins parsed taken = do
      putStrLn $ "parsed: " <> show parsed
      putStrLn $ "taken:  " <> show taken
