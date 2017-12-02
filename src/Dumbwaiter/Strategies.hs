{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
module Dumbwaiter.Strategies
  ( Matcher
  , Responder
  , RouteConfig(..)
  , buildHandler
  ) where


import Data.Text.Lazy (fromStrict)
import qualified Web.Firefly as F
import qualified Data.Map as M
import qualified Data.Text.Lazy.Encoding  as LTE
import qualified Data.Text.Encoding  as TE
import qualified Network.HTTP.Types.Status as S
import qualified Data.CaseInsensitive as CI
import qualified Network.Wai as W
import Control.Monad
import Data.Foldable
import Dumbwaiter.Types

import qualified Network.HTTP.Types.Header as HTTP

toWaiResponse :: ResponseBuilder -> W.Response
toWaiResponse ResponseBuilder{..} = W.responseLBS status headers' content
  where
    content = LTE.encodeUtf8 . fromStrict $ body
    status = S.mkStatus statusCode mempty
    headers' = fromHeaderMap headers

buildHandler :: RouteConfig -> [Matcher] -> [Responder] -> F.App ()
buildHandler RouteConfig{match=matcherConfig, response=respConfig} matchers responders 
  = do
    isMatch <- fmap (all id) . F.runHandler $ traverse ($ matcherConfig) matchers
    when isMatch $ do
      F.respondWith (fmap toWaiResponse . buildResponse $ emptyResponse)
  where
    buildResponse start = foldl' (>>=) (pure start) (fmap ($ respConfig) responders)
    emptyResponse = ResponseBuilder
      { statusCode = 200
      , body = mempty
      , headers = mempty
      }

fromHeaderMap :: F.HeaderMap -> HTTP.ResponseHeaders
fromHeaderMap hm = do
  (headerName, vals) <- M.toList hm
  [(CI.map TE.encodeUtf8 headerName, TE.encodeUtf8 value) | value <- vals]
