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
import System.Exit (exitFailure)
import Control.Monad.IO.Class
import Data.Monoid
import Control.Monad
import Control.Concurrent
import System.FSNotify


data Config = Config
  { routes :: [RouteConfig]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile filepath = first show <$> Y.decodeFileEither filepath

type Port = Int

run :: [Matcher] -> [Responder] -> FilePath -> Port -> IO ()
run matchers responders configFilePath port = do
  putStrLn $ "Serving on port " <> show port
  filewatcher configFilePath (runServer matchers responders port)

runServer :: [Matcher] -> [Responder] -> Int -> [RouteConfig] -> IO ()
runServer matchers responders port routes' =
  F.run port $ do
    path <- F.getPath
    method <- F.getMethod
    liftIO . TIO.putStrLn $ method <> " " <> path
    traverse_ (\routeConf -> buildHandler routeConf matchers responders) routes'

readRoutes :: String -> IO [RouteConfig]
readRoutes configFilePath = do
  config <- readConfigFile configFilePath
  case config of
    Left err -> print err >> exitFailure
    Right Config{routes} -> return routes


filewatcher :: String -> ([RouteConfig] -> IO ()) -> IO ()
filewatcher configFilePath serverRunner = withManager $ \m -> do
  _ <- watchDir m "." matchesConfFile $ \evt -> do
    putStrLn $ eventPath evt
    routes' <- readRoutes configFilePath
    serverRunner routes'
  forever $ threadDelay 1000000 -- block forever
    where
      matchesConfFile = const True -- (== configFilePath) . eventPath
