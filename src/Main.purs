module Main where

import Prelude

import Application (runApp)
import CLI (parseCliArgs)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = do
  env <- parseCliArgs
  launchAff_ $ runApp env
