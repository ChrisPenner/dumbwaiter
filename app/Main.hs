{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# language NamedFieldPuns #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import GHC.Generics
import qualified Data.Aeson as A
import Dumbwaiter
import Dumbwaiter.Matchers
import Dumbwaiter.Responders

data Options = Options
  { configFile :: String
  , port :: Int
  } deriving (Generic, A.FromJSON, A.ToJSON)


options :: Parser Options
options = Options
      <$> strOption
          ( long "config-file"
         <> short 'f'
         <> metavar "CONFIG_FILE"
         <> help "The file containing a yaml configuration" )
      <*> option auto
          ( long "port"
         <> short 'p'
         <> metavar "PORT"
         <> help "The port to run the server on" )

main :: IO ()
main = execParser opts >>= \Options{configFile, port} -> run allMatchers allResponders configFile port
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Spin up a simple webserver according to your config")
