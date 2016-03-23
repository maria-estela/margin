import Data.ByteString.Lazy( readFile )
import Margin( value, defaultFileName )
import Data.Aeson( eitherDecode )
import Control.Applicative( (<$>) )

main = do
  eitherDecoded <- eitherDecode <$> Data.ByteString.Lazy.readFile ("./" ++ defaultFileName)
  putStrLn $ either id (\x -> show $ sum (map value x)) eitherDecoded
