module Application where

import Prelude

import CLI (Command(..), Env, LibraryDepsOptions, SpagoFilesOptions)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (difference, elemIndex, foldl, intercalate, length, nub, snoc, sort, sortBy, uncons)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, filterKeys, lookup, toArrayBy)
import Data.HashMap as HashMap
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (power)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String.CodeUnits (drop, take)
import Data.String.Utils (padEnd)
import Data.Traversable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdirRecursive, readTextFile, writeTextFile)
import Parser (parsePackageSetJson)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.StringParser (unParser)
import Types (PackageMeta)

runApp :: Env -> Aff Unit
runApp env = do
  packageJson <- readTextFile UTF8 env.input
  case unParser parsePackageSetJson {str: packageJson, pos:0} of
    Left e ->
      liftEffect do
        let width = 10
        log $ show e.error <> " @ " <> show e.pos
        log $ take (width * 2) $ drop (e.pos - width) packageJson
        log $ power " " (width - 1) <> "^"
    Right { result } -> do
      case env.command of
        GenLibraryDeps options -> do
          runGenLibDeps options result
        GenSpagoFiles options -> do
          runSpagoFiles options $ findAllTransitiveDeps result

  where
    runGenLibDeps :: LibraryDepsOptions -> HashMap String PackageMeta -> Aff Unit
    runGenLibDeps { libraryDepFile, finishedDepsFile } result = do
      fileExists <- exists libraryDepFile
      if fileExists && not env.force
        then liftEffect do
          log $
              "Error: Output file '" <> libraryDepFile <> "'already exists. \
              \To overwrite this file, use the `--force` flag."
          log "Exiting program."
        else do
          finishedDeps <- readTextFile UTF8 finishedDepsFile
          let depsToRemove = map trim $ split (Pattern "\n") finishedDeps
          let removedDeps = removeFinishedDeps depsToRemove result
          let allDepsKnown = findAllTransitiveDeps removedDeps
          let orderedContent = mkOrderedContent $ mkSortedPackageArray allDepsKnown
          writeTextFile UTF8 libraryDepFile orderedContent

    runSpagoFiles :: SpagoFilesOptions -> HashMap String PackageMeta -> Aff Unit
    runSpagoFiles { directory, whitelist } allDepsKnown = do
      dirExists <- exists directory
      unless dirExists $ mkdirRecursive directory
      let
        onlyDesiredPackages = case whitelist of
          Nothing -> allDepsKnown
          Just packagesToInclude -> do
            let isDesiredPackage p = isJust (p `elemIndex` packagesToInclude)
            filterKeys isDesiredPackage allDepsKnown
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

removeFinishedDeps :: Array String -> HashMap String PackageMeta -> HashMap String PackageMeta
removeFinishedDeps depsToRemove packageMap = do
  let removedKeys = foldl (flip HashMap.delete) packageMap depsToRemove
  removedKeys <#> \packageMeta ->
    packageMeta { dependencies = difference packageMeta.dependencies depsToRemove }

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
    let
      depCount = padEnd 2 $ show r.depCount
      package = padEnd 30 r.package
      repo = padEnd 90 r.meta.repo
      nextLine = depCount <> " " <> package <> " " <> repo <> " " <> show r.meta.dependencies
    in { init: false
       , str: if acc.init then nextLine else acc.str <> "\n" <> nextLine
       }

mkSpagoDhall :: forall r. { package :: String, meta :: PackageMeta | r } -> String
mkSpagoDhall {package, meta } = joinWith "\n"
  [ "{ name = \"my-project\""
  , ", dependencies = " <> show (meta.dependencies `snoc` package)
  , ", packages = ./packages.dhall"
  , ", sources = [] : List Text"
  , "}"
  ]
