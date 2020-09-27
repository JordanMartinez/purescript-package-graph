module Main where

import Prelude

import ChildProcess (execSync)
import Data.Either (Either(..))
import Data.Monoid (power)
import Data.String.CodeUnits (drop, take)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parser (parsePackageSetJson)
import Text.Parsing.StringParser (unParser)

main :: Effect Unit
main = launchAff_ do
  let packageJsonFileName = "./packageSet.json"
  liftEffect do
    execSync $ "dhall-to-json --compact --file ./packages.dhall --output " <> packageJsonFileName

  packageJson <- readTextFile UTF8 packageJsonFileName
  case unParser parsePackageSetJson {str: packageJson, pos:0} of
    Left e ->
      liftEffect do
        log $ show e.error <> " @ " <> show e.pos
        log $ take 20 $ drop (e.pos - 10) packageJson
        log $ power " " 9 <> "^"
    Right _ -> liftEffect $ log "success"
