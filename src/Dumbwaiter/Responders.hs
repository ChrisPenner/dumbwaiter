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
import Data.Text.Lens (packed)
import Data.Monoid
import Dumbwaiter.Types
import Control.Monad.IO.Class
import Control.Exception
import System.IO
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Web.Firefly as F
import qualified Data.Text.IO as TIO

allResponders  :: [Responder]
allResponders =
  [ bodyResponder
  , headerResponder
  , statusResponder
  , fileResponder
  , staticFiles
  ]

bodyResponder :: Responder
bodyResponder respConfig = case body' of
                             Nothing -> pure
                             Just content -> pure . (body <>~ content)
  where
    body' = respConfig ^? key "body" . _String

headerResponder :: Responder
headerResponder respConfig = pure . (headers <>~ headerMap)
  where
    headerMap = fmap pure . M.mapKeys CI.mk . M.fromList $ respConfig ^@.. key "headers" . members . _String

statusResponder :: Responder
statusResponder respConfig = pure . (statusCode .~ fromIntegral status)
  where
    status = fromMaybe 200 $ respConfig ^? key "status" . _Integer

fileResponder :: Responder
fileResponder respConfig builder =
  case filepath of
    Nothing -> pure builder
    Just path -> liftIO $ addFile path `onException` printError path
  where
    filepath = respConfig ^? key "file" . _String . from packed
    printError path = do
      hPutStrLn stderr $ "File not found: " <> path
      return $ builder & body .~ "Internal Server Error"
                       & statusCode .~ 500
    addFile path = do
      content <- TIO.readFile path
      return $ builder & body <>~ content

staticFiles :: Responder
staticFiles respConfig builder = 
  case dir of
    Nothing -> pure builder
    Just d -> do
      path <- dropWhile (== '/') . T.unpack <$> F.getPath
      contents <- liftIO . TIO.readFile $ d </> path
      return $ builder & body <>~ contents
  where
    dir = respConfig ^? key "static-files" . _String . from packed
