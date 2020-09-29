module Main where

import Prelude

import ChildProcess (execSync)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM)
import Data.Array (foldl, intercalate, length, nub, snoc, sort, sortBy, uncons)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, lookup, toArrayBy)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parser (parsePackageSetJson)
import Partial.Unsafe (unsafeCrashWith)
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
        let width = 10
        log $ show e.error <> " @ " <> show e.pos
        log $ take (width * 2) $ drop (e.pos - width) packageJson
        log $ power " " (width - 1) <> "^"
    Right { result } -> do
      let
        allDepsKnown = findAllTransitiveDeps result
        sortedPackageArray = mkSortedPackageArray allDepsKnown
        orderedContent = mkOrderedContent sortedPackageArray

      writeTextFile UTF8 "./orderedContent.txt" $ orderedContent
      for_ sortedPackageArray \rec -> do
        let
          filePath = "./spagoFiles/" <> rec.package <> ".dhall"
          fileContent = mkSpagoDhall rec
        writeTextFile UTF8 filePath fileContent

findAllTransitiveDeps :: HashMap String PackageMeta -> HashMap String PackageMeta
findAllTransitiveDeps packageMap = foldlWithIndex buildMap HashMap.empty packageMap
  where
  buildMap :: String -> HashMap String PackageMeta -> PackageMeta -> HashMap String PackageMeta
  buildMap packageName mapSoFar packageMeta = do
    case lookup packageName mapSoFar of
      Just deps -> mapSoFar
      Nothing ->
        let { deps, updatedMap } = getDepsRecursively packageName packageMeta mapSoFar
        in updatedMap

  getDepsRecursively :: String -> PackageMeta -> HashMap String PackageMeta
    -> { deps :: Array String, updatedMap :: HashMap String PackageMeta }
  getDepsRecursively packageName packageMeta mapSoFar = do
    let
      direct = getDeps packageName
    tailRec go { packageName, packageMeta, mapSoFar, allDeps: direct, remaining: direct }

  go :: _ -> Step _ _
  go state@{ packageName, packageMeta, mapSoFar, allDeps, remaining } = case uncons remaining of
    Nothing ->
      let
        allDepsNubbed = nub allDeps
        newMeta = packageMeta { dependencies = allDepsNubbed }
      in Done { deps: allDepsNubbed, updatedMap: mapSoFar <> (HashMap.singleton packageName newMeta) }
    Just { head: package, tail } -> do
      case lookup packageName mapSoFar of
        Just newMeta -> Loop $ state { allDeps = nub $ state.allDeps <> newMeta.dependencies, remaining = tail }
        Nothing ->  case lookup package packageMap of
          Nothing ->
            unsafeCrashWith $
              "The impossible happened. `packageMap` does not contain \
              \the package '" <> packageName <> "'."
          Just oldMeta ->
            let { deps, updatedMap } = getDepsRecursively package oldMeta mapSoFar
            in Loop $ state { allDeps = nub $ allDeps <> deps, mapSoFar = updatedMap, remaining = tail }

  getDeps :: String -> Array String
  getDeps packageName =
    fromMaybe [] $ map (_.dependencies) $ lookup packageName packageMap

mkSortedPackageArray :: HashMap String PackageMeta -> Array { package :: String, meta :: PackageMeta, depCount :: Int }
mkSortedPackageArray =
  toArrayBy (\k v ->
      { package: k, meta: v { dependencies = sort v.dependencies }, depCount: length v.dependencies})
    >>> sortBy (\l r ->
        case compare l.depCount r.depCount of
          EQ -> compare l.package r.package
          x -> x
    )

mkOrderedContent :: Array { package :: String, meta :: PackageMeta, depCount :: Int } -> String
mkOrderedContent arr = foldResult.str
  where
    foldResult = foldl buildLine {init: true, str: ""} arr
    buildLine acc r =
      let nextLine = show r.depCount <> "-" <> r.package <> ": " <> show r.meta.dependencies
      in { init: false
         , str: if acc.init then nextLine else acc.str <> "\n" <> nextLine
         }

mkSpagoDhall :: forall r. { package :: String, meta :: PackageMeta | r } -> String
mkSpagoDhall {package, meta } = joinWith "\n"
  [ "{ name = \"my-project\""
  , ", dependencies = " <> show (meta.dependencies `snoc` package)
  , ", packages = ./packages.dhall"
  , ", sources = [ \"src/**/*.purs\" ]"
  , "}"
  ]
