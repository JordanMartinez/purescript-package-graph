module CLI where

import Prelude

import Data.Array (filter, nub)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as String
import Effect (Effect)
import ExitCodes (ExitCode(..))
import Options.Applicative (execParser, failureCode, footer, help, helper, info, long, metavar, option, progDesc, short, showDefault, strOption, switch, value)
import Options.Applicative as OA
import Options.Applicative.Types (ReadM, readerAsk)

type Env =
  { input :: String
  , force :: Boolean
  , command :: Command
  }

data Command
  = GenLibraryDeps LibraryDepsOptions
  | GenSpagoFiles SpagoFilesOptions

type LibraryDepsOptions =
  { libraryDepFile :: String
  }

type SpagoFilesOptions =
  { directory :: String
  , whitelist :: Maybe (Array String)
  }

newtype BlacklistPackages = Blacklist (Array String)
newtype WhitelistPackages = WhiteList (Array String)

parseCliArgs :: Effect Env
parseCliArgs = do
  execParser $ OA.info (helper <*> parser) $ fold
    [ progDesc
        $ "Given a JSON version of a package set's `packages.dhall` file, \
          \does one of the following:\n\
          \1. `genLibDeps` - " <> genLibsDescription <> "\n\
          \2. `genSpagoFiles` - " <> genSpagoFilesDescription
    , failureCode Error
    , footer "Example usage: package-query --input ./packageSet.json --force \
             \genSpagoFiles --dir ./spagoFiles --whitelist foldable-traversable"
    ]

genLibsDescription :: String
genLibsDescription =
  "Generates a file that contains all packages \
  \in order of how many dependencies they have (starting with 0). \
  \Use this to determine the most efficient order to update the \
  \ecosystem."

genSpagoFilesDescription :: String
genSpagoFilesDescription =
  "Generates a `spago.dhall` file named \
  \`<packageName>.dhall` for each package in the package set. To only output \
  \such files for specific packages, use the `whitelist` argument. Use this via \
  \`spago -x <packageName>.dhall build` to install and build \
  \only that package and all of its dependencies."

parser :: OA.Parser Env
parser = ado
  input <- strOption $ fold
    [ long "input"
    , short 'i'
    , metavar "FILE"
    , help "The JSON-version of the `packages.dhall` file. Use \
           \`dhall-to-json --compact --file ./packages.dhall \
           \--output ./packageSet.json` to produce this file."
    ]
  force <- switch $ fold
    [ long "force"
    , short 'f'
    , help "Overwrite any files that already exist."
    ]
  command <- commandParser
  in { input, force, command }

commandParser :: OA.Parser Command
commandParser = OA.subparser $ fold
  [ OA.command "genLibDeps"
      $ info (helper <*> parseGenLibDeps)
      $ progDesc genLibsDescription
  , OA.command "genSpagoFiles"
      $ info (helper <*> parseGenSpagoFiles)
      $ progDesc genSpagoFilesDescription
  ]

parseGenLibDeps :: OA.Parser Command
parseGenLibDeps = map GenLibraryDeps ado
  libraryDepFile <- strOption $ fold
    [ long "output"
    , short 'l'
    , metavar "FILE"
    , help "Indicates the file that will store the outputted content."
    , value "./ordered-content.txt"
    , showDefault
    ]
  in { libraryDepFile }

parseGenSpagoFiles :: OA.Parser Command
parseGenSpagoFiles = map GenSpagoFiles ado
  directory <- strOption $ fold
    [ long "dir"
    , metavar "DIRECTORY"
    , short 's'
    , help "Indicates the directory which will store the outputted \
           \`<packageName>.dhall` files."
    , value "./spagoFiles"
    , showDefault
    ]
  whitelist <- option maybeMultiString $ fold
    [ long "whitelist"
    , short 'w'
    , metavar "PACKAGE1,PACKAGE2,...,PACKAGEn"
    , help "Only produce `spago.dhall` config files for the specified \
           \list of packages. Packages must be separated by a comma \
           \with no spaces in-between."
    ]

  in { directory, whitelist }

maybeMultiString :: ReadM (Maybe (Array String))
maybeMultiString = do
  s <- readerAsk
  let strArray = filter (not <<< String.null) $ split (Pattern ",") s
  if Array.null strArray
    then pure Nothing
    else pure $ Just $ nub strArray
