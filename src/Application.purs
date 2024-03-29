module Application where

import Prelude

import CLI (Command(..), Env, LibraryDepsOptions)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (difference, foldl, length, snoc, sort, sortBy, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String.CodeUnits as SCU
import Data.String.Utils (padEnd)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, readTextFile, writeTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Types (PackageMeta, PackageInfo)

runApp :: Env -> Aff Unit
runApp env = do
  packageJson <- readTextFile UTF8 env.input
  let
    decode j = lmap printJsonDecodeError do
      obj :: Object Json <- decodeJson j
      keyVals <- forWithIndex obj \key val -> do
        packageMeta :: PackageMeta <- decodeJson val
        pure $ Tuple key packageMeta
      pure $ Map.fromFoldable keyVals
  case decode =<< jsonParser packageJson of
    Left e -> liftEffect do
      log $ "Decoding error while parsing JSON file"
      log e
    Right result -> do
      case env.command of
        GenLibraryDeps options -> do
          runGenLibDeps options result
        GenSpagoFiles options -> do
          pure unit
  -- runSpagoFiles options $ findAllTransitiveDeps result

  where
  runGenLibDeps :: LibraryDepsOptions -> Map String PackageMeta -> Aff Unit
  runGenLibDeps { libraryDepFile, finishedDepsFile } result = do
    fileExists <- exists libraryDepFile
    if fileExists && not env.force then liftEffect do
      log $
        "Error: Output file '" <> libraryDepFile <>
          "'already exists. \
          \To overwrite this file, use the `--force` flag."
      log "Exiting program."
    else do
      finishedDeps <- readTextFile UTF8 finishedDepsFile
      let
        depsToRemove = map trim $ split (Pattern "\n") finishedDeps
        removedDeps = removeFinishedDeps depsToRemove result
        allDepsKnown = findAllTransitiveDeps removedDeps
        orderedContent = mkOrderedContent $ mkSortedPackageArray allDepsKnown
      writeTextFile UTF8 libraryDepFile orderedContent

-- runSpagoFiles :: SpagoFilesOptions -> Map String (PackageMeta) -> Aff Unit
-- runSpagoFiles { directory, whitelist } allDepsKnown = do
--   dirExists <- exists directory
--   unless dirExists $ mkdirRecursive directory
--   let
--     onlyDesiredPackages = case whitelist of
--       Nothing -> allDepsKnown
--       Just packagesToInclude -> do
--         let isDesiredPackage p = isJust (p `elemIndex` packagesToInclude)
--         filterKeys isDesiredPackage allDepsKnown
--     sortedPackageArray = mkSortedPackageArray onlyDesiredPackages
--   ref <- liftEffect $ Ref.new Nil
--   for_ sortedPackageArray \rec -> do
--     let
--       filePath = directory <> "/" <> rec.package <> ".dhall"
--       fileContent = mkSpagoDhall rec
--     fileExists <- exists filePath
--     when (fileExists && not env.force) do
--       liftEffect $ log $
--         "spago.dhall file for package '" <> rec.package <>
--           "' already \
--           \exists. Skipping this file."
--     writeTextFile UTF8 filePath fileContent
--   liftEffect do
--     list <- Ref.read ref
--     unless (List.null list) do
--       log $
--         "The following packages did not have a \
--         \`spago.dhall` file created because they \
--         \already exist. To overwrite them, use the \
--         \`--force` flag."
--       let content = intercalate ", " list
--       log content

findAllTransitiveDeps :: Map String PackageMeta -> Map String PackageInfo
findAllTransitiveDeps packageMap = foldlWithIndex buildMap Map.empty packageMap
  where
  buildMap :: String -> Map String PackageInfo -> PackageMeta -> Map String PackageInfo
  buildMap packageName mapSoFar packageMeta = do
    case lookup packageName mapSoFar of
      Just _ -> mapSoFar
      Nothing ->
        let
          { updatedMap } = getDepsRecursively packageName packageMeta mapSoFar
        in
          updatedMap

  getDepsRecursively
    :: String
    -> PackageMeta
    -> Map String PackageInfo
    -> { deps :: Set String, updatedMap :: Map String PackageInfo }
  getDepsRecursively packageName packageMeta mapSoFar = do
    let
      direct = getDeps packageName
    tailRec go { packageName, packageMeta, mapSoFar, allDeps: direct, remaining: Set.toUnfoldable direct }

  go :: _ -> Step _ _
  go state@{ packageName, packageMeta, mapSoFar, allDeps, remaining } = case uncons remaining of
    Nothing ->
      let
        newMeta = packageMeta { dependencies = allDeps }
      in
        Done { deps: allDeps, updatedMap: Map.union mapSoFar (Map.singleton packageName newMeta) }
    Just { head: package, tail } -> do
      case lookup packageName mapSoFar of
        Just newMeta -> Loop $ state { allDeps = state.allDeps <> newMeta.dependencies, remaining = tail }
        Nothing -> case lookup package packageMap of
          Nothing ->
            unsafeCrashWith $
              "The impossible happened. `packageMap` does not contain \
              \the package '" <> packageName <> "'."
          Just oldMeta ->
            let
              { deps, updatedMap } = getDepsRecursively package oldMeta mapSoFar
            in
              Loop $ state { allDeps = allDeps <> deps, mapSoFar = updatedMap, remaining = tail }

  getDeps :: String -> Set String
  getDeps packageName =
    fromMaybe Set.empty $ map (Set.fromFoldable <<< _.dependencies) $ lookup packageName packageMap

removeFinishedDeps :: Array String -> Map String PackageMeta -> Map String PackageMeta
removeFinishedDeps depsToRemove packageMap = do
  let removedKeys = foldl (flip Map.delete) packageMap depsToRemove
  removedKeys <#> \packageMeta ->
    packageMeta { dependencies = difference packageMeta.dependencies depsToRemove }

mkSortedPackageArray :: Map String PackageInfo -> Array { package :: String, meta :: PackageMeta, depCount :: Int }
mkSortedPackageArray =
  Map.toUnfoldable
    >>> map
      ( \(Tuple k v) -> do
          let depArr = Set.toUnfoldable v.dependencies
          { package: k, meta: v { dependencies = sort depArr }, depCount: length depArr }
      )
    >>> sortBy
      ( \l r ->
          case compare l.depCount r.depCount of
            EQ -> compare l.package r.package
            x -> x
      )

mkOrderedContent :: Array { package :: String, meta :: PackageMeta, depCount :: Int } -> String
mkOrderedContent arr = foldResult.str
  where
  maxLength = foldl maxPartLength { dep: 0, package: 0, repo: 0 } arr
  maxPartLength acc r =
    { dep: max acc.dep $ SCU.length $ show r.depCount
    , package: max acc.package $ SCU.length r.package
    , repo: max acc.repo $ SCU.length r.meta.repo
    }
  foldResult = foldl buildLine { init: true, str: "" } arr
  buildLine acc r =
    let
      depCount = padEnd maxLength.dep $ show r.depCount
      package = padEnd maxLength.package r.package
      repo = padEnd maxLength.repo r.meta.repo
      nextLine = depCount <> " " <> package <> " " <> repo <> " " <> show r.meta.dependencies
    in
      { init: false
      , str: if acc.init then nextLine else acc.str <> "\n" <> nextLine
      }

mkSpagoDhall :: forall r. { package :: String, meta :: PackageMeta | r } -> String
mkSpagoDhall { package, meta } = joinWith "\n"
  [ "{ name = \"my-project\""
  , ", dependencies = " <> show (meta.dependencies `snoc` package)
  , ", packages = ./packages.dhall"
  , ", sources = [] : List Text"
  , "}"
  ]
