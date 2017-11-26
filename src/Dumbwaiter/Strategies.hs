{-# language NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language OverloadedStrings #-}
module Dumbwaiter.Strategies
  ( Strategy
  , simpleRoute
  ) where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Web.Firefly as F
import qualified Data.Map as M
import Data.Maybe
import qualified Network.HTTP.Types.Status as S
import qualified Data.CaseInsensitive as CI

type Strategy = A.Value -> Maybe (F.Handler ())

data SimpleRoute =
  SimpleRoute
    { route :: T.Text
    , response :: T.Text
    , statusCode :: Maybe Int
    , headers :: Maybe (M.Map T.Text T.Text)
    } deriving (Show,Generic,  A.FromJSON, A.ToJSON)

simpleRoute :: Strategy
simpleRoute val =
    case A.fromJSON val of
      A.Error _ -> Nothing
      A.Success SimpleRoute{route, response, statusCode, headers} ->
        let status = S.mkStatus <$> statusCode <*> pure ""
            headerMap = mkHeaderMap headers
         in
          Just $ F.route route (pure (response, fromMaybe S.ok200 status, headerMap))
  where
    mkHeaderMap :: Maybe (M.Map T.Text T.Text) -> M.Map (CI.CI T.Text) [T.Text]
    mkHeaderMap = fmap pure . M.mapKeys CI.mk . fromMaybe M.empty
