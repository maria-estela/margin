import Data.List( uncons, intersperse )
import System.IO
import System.Environment( getArgs )
import Text.Read( readMaybe )
import Margin
import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options {
  optVal    :: Float,
  optDes    :: Maybe String,
  optMargin :: Maybe String
  }

options :: Parser Options
options = Options
          <$> argument auto (metavar "value")
          <*> optional (argument str (metavar "description"))
          <*> optional (option str (long "margin-data"))

defDesc :: Options -> (Float, String)
defDesc o =
  let d = maybe "" id (optDes o)
  in (optVal o, d)

main = do
  opts <- execParser (info options fullDesc)
  addToMaybeFile (optMargin opts) (defDesc opts)
