module Main where

import Prelude

import ChildProcess (execSync)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (foldl, intercalate, length, nub, snoc, sort, sortBy, uncons)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.HashMap (HashMap, lookup, toArrayBy)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (power)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (for_)
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
        mkSortedPackageArray =
          toArrayBy (\k v ->
              { package: k, meta: v { dependencies = sort v.dependencies }, depCount: length v.dependencies})
            >>> sortBy (\l r ->
                case compare l.depCount r.depCount of
                  EQ -> compare l.package r.package
                  x -> x
            )

        sortedPackageArray = mkSortedPackageArray allDepsKnown

        mkOrderedContent =
            flip foldl {init: true, str: ""} \acc r ->
              let nextLine = show r.depCount <> "-" <> r.package <> ": " <> show r.meta.dependencies
              in { init: false
                 , str: if acc.init then nextLine else acc.str <> "\n" <> nextLine
                 }

        orderedContent = (mkOrderedContent sortedPackageArray).str

      writeTextFile UTF8 "./orderedContent.txt" $ orderedContent
      for_ sortedPackageArray \rec -> do
        let
          filePath = "./spagoFiles/" <> rec.package <> ".sh"
          fileContent = mkSpagoDhall rec
        writeTextFile UTF8 filePath fileContent

findAllTransitiveDeps :: HashMap String PackageMeta -> HashMap String PackageMeta
findAllTransitiveDeps packageMap = foldlWithIndex buildMap HashMap.empty packageMap
  where
  buildMap :: String -> HashMap String PackageMeta -> PackageMeta -> HashMap String PackageMeta
  buildMap packageName acc packageMeta =
    let
      deps = getDepsRecursively packageName
      newMeta = packageMeta { dependencies = nub (packageMeta.dependencies <> deps)}
    in
      acc <> (HashMap.singleton packageName newMeta)

  getDepsRecursively :: String -> Array String
  getDepsRecursively packageName =
    let
      direct = getDeps packageName
      transitive = tailRec go {acc: [], remaining: direct }
    in
      nub $ direct <> transitive

  go :: _ -> Step _ _
  go { acc, remaining } = case uncons remaining of
    Nothing ->
      Done acc
    Just { head, tail } ->
      Loop { acc: nub $ acc <> getDepsRecursively head, remaining: tail }

  getDeps :: String -> Array String
  getDeps packageName =
    fromMaybe [] $ map (_.dependencies) $ lookup packageName packageMap

mkSpagoDhall :: forall r. { package :: String, meta :: PackageMeta | r } -> String
mkSpagoDhall {package, meta } = do
  let
    firstPart = "node ../../purescript-psa --purs=./purs-v0.14-rc2 --strict "
    allPackages = meta.dependencies `snoc` package
    globs = intercalate " " $ allPackages <#> \p ->
      "\".spago/" <> p <> "/*/src/**/*.purs"
  firstPart <> globs
