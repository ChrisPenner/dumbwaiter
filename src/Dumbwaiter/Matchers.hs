{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
module Dumbwaiter.Matchers
  ( allMatchers
  ) where

import qualified Data.Text as T
import qualified Web.Firefly as F
import Data.Aeson.Lens
import Control.Lens
import Dumbwaiter.Types

allMatchers :: [Matcher]
allMatchers = 
  [ methodMatcher
  , routeMatcher
  ]

methodMatcher :: Matcher
methodMatcher val = case method of
                      Nothing -> return True
                      Just m -> (==m) . T.toLower <$> F.getMethod
    where
      method = T.toLower <$> val ^? key "method" . _String

routeMatcher :: Matcher
routeMatcher val = case route of
                     Nothing -> return True
                     Just r -> F.pathMatches r
  where
    route = val ^? key "path" . _String


