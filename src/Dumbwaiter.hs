{-# language NamedFieldPuns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language OverloadedStrings #-}
module Dumbwaiter
  ( run
  ) where

import Dumbwaiter.Strategies

import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import Data.Bifunctor
import Data.Foldable
import qualified Web.Firefly as F
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class
import Data.Monoid
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import System.FSNotify
import System.Directory
import System.IO


data Config = Config
  { routes :: [RouteConfig]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile filepath = first Y.prettyPrintParseException <$> Y.decodeFileEither filepath

type Port = Int

run :: [Matcher] -> [Responder] -> FilePath -> Port -> IO ()
run matchers responders configFilePath port = do
  putStrLn $ "Serving on port " <> show port
  filewatchLoop configFilePath (runServer matchers responders port)

runServer :: [Matcher] -> [Responder] -> Int -> [RouteConfig] -> IO ()
runServer matchers responders port routes' =
  F.run port $ do
    path <- F.getPath
    method <- F.getMethod
    liftIO . TIO.putStrLn $ method <> " " <> path
    traverse_ (\routeConf -> buildHandler routeConf matchers responders) routes'

readRoutes :: String -> IO (Either String [RouteConfig])
readRoutes configFilePath = fmap routes <$> readConfigFile configFilePath

filewatchLoop :: String -> ([RouteConfig] -> IO ()) -> IO ()
filewatchLoop configFilePath serverRunner = withManager $ \m -> do
  absConfigPath <- makeAbsolute configFilePath
  let matchesConfFile = (== absConfigPath) . eventPath
  syncVar <- newEmptyMVar
  _ <- forkIO $ loadAndRunServer syncVar
  _ <- watchDir m "." matchesConfFile $ const (putMVar syncVar ())
  forever $ threadDelay 1000000 -- block forever
    where
      loadAndRunServer syncVar = forever $ do
        routes' <- readRoutes configFilePath
        case routes' of
          Left err -> hPutStrLn stderr err >> takeMVar syncVar
          Right rs -> race_ (serverRunner rs) (takeMVar syncVar)
        putStrLn $ configFilePath <> " changed, restarting server..."
