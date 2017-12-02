{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language OverloadedStrings #-}
module Dumbwaiter.Types where

import GHC.Generics
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import qualified Data.Aeson as A
import qualified Web.Firefly as F
import qualified Data.Map as M
import qualified Data.Text.Lazy.Encoding  as LTE
import qualified Data.Text.Encoding  as TE
import Data.Maybe
import qualified Network.HTTP.Types.Status as S
import qualified Data.CaseInsensitive as CI
import Data.Aeson.Lens
import Control.Lens
import qualified Network.Wai as W
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Data.Foldable

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

