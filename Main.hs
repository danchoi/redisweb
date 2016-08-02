{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
module Main where
import Options.Applicative
import qualified Database.Redis.Redis as R
import Database.Redis.Redis (Redis)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.Wai (requestHeaders)
import Web.Scotty hiding (header)
import Network.Wai.Handler.Warp (run)
import Data.Time.Format
import Data.Time.Clock
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Applicative
import Data.Monoid
import Data.List (intersperse)
import Data.Maybe
import System.IO

data Options' = Options' {
        verbose :: Bool
      , port :: Int
      , queue :: String
      , path :: String
      , paramNames :: [TL.Text]
      } deriving Show

options' :: Parser Options'
options' = Options' 
    <$> flag False True (short 'v' <> help "Verbose: output parsed options.")
    <*> argument auto (metavar "PORT")
    <*> strArgument (metavar "QUEUE" <> help "Redis queue name")
    <*> strArgument (metavar "PATH" <> help "HTTP PATH for POST request")
    <*> many (
          TL.pack <$> strArgument (metavar "PARAM-NAME" <> help "Post param field name")
        )

opts :: ParserInfo Options'
opts = info (helper <*> options') (fullDesc <> header "redisweb")

main :: IO ()
main = do
  o@Options'{..} <- execParser opts
  when verbose $ hPutStrLn stderr $ show o

  redisConn <- R.connect R.localhost R.defaultPort
  app <- scottyApp $ do
    get (capture path) $ do

      headers  <- requestHeaders <$> request
      let ip :: B8.ByteString
          ip = fromMaybe "127.0.0.1" $ lookup "X-Real-IP" headers
      xs <- mapM param paramNames
      time <- liftIO $ fmap (formatTime defaultTimeLocale "%FT%X%z") 
                       getCurrentTime
      let message = mconcat 
                        $ intersperse " " 
                        $ xs <> [ip , B8.pack time] 
      liftIO $ R.lpush redisConn queue (BL8.fromChunks [message])
      text "OK"
  run port app

