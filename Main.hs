module Main where
import Options.Applicative
import qualified Database.Redis.Redis as R
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

main :: IO ()
main = do
  putStrLn "hello world"
