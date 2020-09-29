module Main where

import Prelude

import CLI (Command(..), parseCliArgs)
import ChildProcess (execSync)
import Control.Monad.Rec.Class (Step(..), tailRec, tailRecM)
import Data.Array (elemIndex, foldl, intercalate, length, nub, snoc, sort, sortBy, uncons)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, filterKeys, lookup, toArrayBy)
import Data.HashMap as HashMap
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, readTextFile, writeTextFile)
import Parser (parsePackageSetJson)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.StringParser (unParser)
import Types (PackageMeta)

main :: Effect Unit
main = do
  env <- parseCliArgs
  launchAff_ do
    packageJson <- readTextFile UTF8 env.input
    case unParser parsePackageSetJson {str: packageJson, pos:0} of
      Left e ->
        liftEffect do
          let width = 10
          log $ show e.error <> " @ " <> show e.pos
          log $ take (width * 2) $ drop (e.pos - width) packageJson
          log $ power " " (width - 1) <> "^"
      Right { result } -> do
        let allDepsKnown = findAllTransitiveDeps result
        case env.command of
          GenLibraryDeps { libraryDepFile } -> do
            fileExists <- exists libraryDepFile
            when (fileExists && not env.force) do
              liftEffect $ throw $ "Output file '" <> libraryDepFile <> "'already exists. \
                    \To overwrite this file, use the `--force` flag."

            let orderedContent = mkOrderedContent $ mkSortedPackageArray allDepsKnown
            writeTextFile UTF8 libraryDepFile orderedContent

          GenSpagoFiles { directory, whitelist } -> do
            let
              onlyDesiredPackages = case whitelist of
                Nothing -> allDepsKnown
                Just packagesToInclude ->
                  allDepsKnown `flip filterKeys` \packageName ->
                    isJust (packageName `elemIndex` packagesToInclude)
              sortedPackageArray = mkSortedPackageArray onlyDesiredPackages
            ref <- liftEffect $ Ref.new Nil
            for_ sortedPackageArray \rec -> do
              let
                filePath = directory <> "/" <> rec.package <> ".dhall"
                fileContent = mkSpagoDhall rec
              fileExists <- exists filePath
              when (fileExists && not env.force) do
                liftEffect $ log $
                  "spago.dhall file for package '" <> rec.package <> "' already \
                  \exists. Skipping this file."
              writeTextFile UTF8 filePath fileContent
            liftEffect do
              list <- Ref.read ref
              unless (List.null list) do
                log $ "The following packages did not have a \
                                 \`spago.dhall` file created because they \
                                 \already exist. To overwrite them, use the \
                                 \`--force` flag."
                let content = intercalate ", " list
                log content

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
