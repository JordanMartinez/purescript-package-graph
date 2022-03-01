module Main where

import Prelude

import Application (runApp)
import ArgParse.Basic (ArgError(..), ArgErrorMsg(..), printArgError)
import CLI (parseCliArgs)
import Data.Array (drop)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Node.Process as Process

main :: Effect Unit
main = do
  args <- map (drop 2) Process.argv
  case parseCliArgs args of
    Left e -> do
      log $ printArgError e
      case e of
        ArgError _ ShowHelp ->
          Process.exit 0
        ArgError _ (ShowInfo _) ->
          Process.exit 0
        _ ->
          Process.exit 1
    Right env ->
      launchAff_ $ runApp env
