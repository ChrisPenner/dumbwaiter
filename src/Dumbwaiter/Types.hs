{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language OverloadedStrings #-}
module Dumbwaiter.Types where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Web.Firefly as F

type ResponseConfig = A.Value
type MatcherConfig = A.Value
type Strategy = RouteConfig -> Maybe (F.Handler ())
type Matcher = MatcherConfig -> F.Handler Bool
type Responder = ResponseConfig -> ResponseBuilder -> F.Handler ResponseBuilder


data RouteConfig =
  RouteConfig
    { match :: A.Value
    , response :: A.Value
    } deriving (Show, Generic, A.FromJSON, A.ToJSON)

data ResponseBuilder =
  ResponseBuilder
    { statusCode :: Int
    , body :: T.Text
    , headers :: F.HeaderMap
    }

