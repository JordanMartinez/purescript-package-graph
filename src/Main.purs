module Main where

import Prelude

import ChildProcess (execSync)
import Control.Alt ((<|>))
import Data.Array (catMaybes, foldl, length, nub, sort, sortBy)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, insertWith, lookup, toArrayBy, update, upsert)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parser (parsePackageSetJson)
import Text.Parsing.StringParser (unParser)
import Types (PackageMeta)

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
    Right { result } -> do
      let
        allDepsKnown = findAllTransitiveDeps result
        mkOrderedContent =
          toArrayBy (\k v ->
              { package: k, deps: sort v.dependencies, depCount: length v.dependencies})
            >>> sortBy (\l r -> compare l.depCount r.depCount)
            >>> flip foldl {init: true, str: ""} \acc r ->
              let nextLine = show r.depCount <> "-" <> r.package <> ": " <> show r.deps
              in { init: false
                 , str: if acc.init then nextLine else acc.str <> "\n" <> nextLine
                 }

      writeTextFile UTF8 "./orderedContent.txt" $ (mkOrderedContent allDepsKnown).str


findAllTransitiveDeps :: HashMap String PackageMeta -> HashMap String PackageMeta
findAllTransitiveDeps packageMap = foldlWithIndex buildMap packageMap packageMap
  where
  buildMap :: String -> HashMap String PackageMeta -> PackageMeta -> HashMap String PackageMeta
  buildMap packageName acc packageMeta =
    let
      deps = getDepsRecursively packageName
    in
      update
        (\meta -> Just $ meta { dependencies = nub (meta.dependencies <> deps)})
        packageName
        acc

  getDepsRecursively :: String -> Array String
  getDepsRecursively packageName =
    let
      direct = getDeps packageName
      transitive = join $ traverse getDepsRecursively direct
    in
      nub $ direct <> transitive

  getDeps :: String -> Array String
  getDeps packageName =
    fromMaybe [] $ map (_.dependencies) $ lookup packageName packageMap
