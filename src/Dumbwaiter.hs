{-# language NamedFieldPuns #-}
{-# language DataKinds #-}
{-# language GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import System.Exit (exitFailure)
import Data.Monoid
import Data.Maybe


data Config = Config
  { matchers :: [A.Value]
  } deriving (Show, Generic, A.FromJSON, A.ToJSON)

readConfig :: FilePath -> IO (Either String Config)
readConfig filepath = first show <$> Y.decodeFileEither filepath

type Port = Int

run :: (Foldable f) => f Strategy -> FilePath -> Port -> IO ()
run strategies configFile port = do
  config <- readConfig configFile
  ms <- case config of
    Left err -> print err >> exitFailure
    Right conf -> return $ matchers conf
  F.run port $ traverse_ (mkHandler strategies) ms

mkHandler :: (Foldable f) => f Strategy -> A.Value -> F.Handler ()
mkHandler strategies matcher = fromMaybe (pure ()) $ firstMatching
  where
    firstMatching = getFirst $ foldMap (\strat -> First $ strat matcher) strategies
