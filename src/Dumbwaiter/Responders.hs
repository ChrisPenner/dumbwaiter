{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
module Dumbwaiter.Responders
  ( allResponders
  ) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.CaseInsensitive as CI
import Data.Aeson.Lens
import Control.Lens
import Data.Monoid
import Dumbwaiter.Types

allResponders  :: [Responder]
allResponders = 
  [ bodyResponder
  , headerResponder
  , statusResponder
  ]

bodyResponder :: Responder
bodyResponder respConfig = case body' of
                             Nothing -> pure
                             Just content -> pure . addContent content
  where
    body' = respConfig ^? key "body" . _String
    addContent content builder = builder{body=body builder <> content}

headerResponder :: Responder
headerResponder respConfig builder@ResponseBuilder{headers} = pure $ builder{headers = headers <> headerMap}
  where
    headerMap = fmap pure . M.mapKeys CI.mk . M.fromList $ respConfig ^@.. key "headers" . members . _String

statusResponder :: Responder
statusResponder respConfig builder = pure $ builder{statusCode=fromIntegral status}
  where
    status = fromMaybe 200 $ respConfig ^? key "status" . _Integer

