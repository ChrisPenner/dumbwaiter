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
import Control.Monad.Reader
import Data.Monoid


data Config = Config
  { routes :: [RouteConfig]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

readConfig :: FilePath -> IO (Either String Config)
readConfig filepath = first show <$> Y.decodeFileEither filepath

type Port = Int

run :: [Matcher] -> [Responder] -> FilePath -> Port -> IO ()
run matchers responders configFile port = do
  config <- readConfig configFile
  routes' <- case config of
           Left err -> print err >> exitFailure
           Right Config{routes} -> return routes
  F.run port $ do
    path <- F.getPath
    method <- F.getMethod
    liftIO . TIO.putStrLn $ method <> " " <> path
    traverse_ (\routeConf -> buildHandler routeConf matchers responders) routes'
