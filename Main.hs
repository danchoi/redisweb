{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Options.Applicative
import qualified Database.Redis.Redis as R
import Database.Redis.Redis (Redis)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.Wai (requestHeaders, remoteHost)
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
import qualified Data.CaseInsensitive as CI

data IPOpt = RemoteHost | IPHeader String
    deriving Show

data Options' = Options' {
        debug :: Bool
      , port :: Int
      , queue :: String
      , path :: String
      , separator :: String
      , ipOpt :: IPOpt
      , paramNames :: [TL.Text]
      } deriving Show

options' :: Parser Options'
options' = Options' 
    <$> flag False True (short 'v' <> help "Debug mode")
    <*> argument auto (metavar "PORT")
    <*> strArgument (metavar "QUEUE" <> help "Redis queue name")
    <*> strArgument (metavar "PATH" <> help "HTTP PATH for POST request")
    <*> strOption (
             short 'F'
          <> metavar "separator"
          <> help "Separator character for queue message fields. Default TAB"
          <> value "\t"
          )
    <*> pIpOpt
    <*> some (
          TL.pack <$> strArgument (metavar "PARAM-NAME" <> help "Post param field name")
        )

pIpOpt :: Parser IPOpt
pIpOpt = 
  (IPHeader <$> 
      (strOption (short 'h' <> metavar "IP-HEADER-NAME" 
            <> help "Request header with IP address to log. E.g. X-Real-IP. Default is to use REMOTE-HOST")))
  <|> pure RemoteHost


opts :: ParserInfo Options'
opts = info (helper <*> options') (fullDesc <> header "redisweb")

main :: IO ()
main = do
  o@Options'{..} <- execParser opts
  when debug $ hPutStrLn stderr $ show o

  redisConn <- R.connect R.localhost R.defaultPort
  app <- scottyApp $ do
    post (capture path) $ do
      liftIO $ print "test"
      headers  <- requestHeaders <$> request
      ip :: B8.ByteString <- case ipOpt of
                  RemoteHost -> (B8.pack . show . remoteHost) <$> request
                  IPHeader k -> do
                        v <- return $ lookup (CI.mk $ B8.pack k) headers
                        return $ fromMaybe "127.0.0.1" v
      xs <- mapM param paramNames
      time <- liftIO $ fmap (formatTime defaultTimeLocale "%FT%X%z") 
                       getCurrentTime
      let message = mconcat 
                        $ intersperse (B8.pack separator)
                        $ xs <> [ip , B8.pack time] 
      if debug 
        then liftIO . putStrLn . B8.unpack $ message
        else do
            liftIO $ R.lpush redisConn queue (BL8.fromChunks [message])
            return ()
      text "OK"
  run port app

