module CLI where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as Arg
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Maybe (Maybe)

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
  , finishedDepsFile :: String
  }

type SpagoFilesOptions =
  { directory :: String
  , whitelist :: Maybe (Array String)
  }

newtype BlacklistPackages = Blacklist (Array String)
newtype WhitelistPackages = WhiteList (Array String)

parseCliArgs :: Array String -> Either Arg.ArgError Env
parseCliArgs = Arg.parseArgs
  "package-graph"
  "To update..."
  (parser <* Arg.flagHelp)

parser :: ArgParser Env
parser = ado
  input <- inputArg
  force <- forceArg
  command <- commandArg
  in { input, force, command }
  where
  inputArg = Arg.argument [ "--input", "-i" ] desc
    # Arg.unformat "FILE" Right
    where
    desc = fold
      [ "The JSON-version of the `packages.dhall` file. Use "
      , "`dhall-to-json --compact --file ./packages.dhall "
      , "--output ./packageSet.json` to produce this file."
      ]
  forceArg = Arg.flag [ "--force", "-f" ] "Overwrite any files that already exist."
    # Arg.boolean

  commandArg = Arg.choose "command"
    [ genLibsCmd
    -- , genSpagoFilesCmd
    ]
  genLibsCmd = Arg.command [ "generate-operators" ] desc do
    GenLibraryDeps
      <$> cmdArgs
      <* Arg.flagHelp
    where
    desc = fold
      [ "Generates a file that contains all packages "
      , "in order of how many dependencies they have (starting with 0). "
      , "Use this to determine the most efficient order to update the "
      , "ecosystem."
      ]

    cmdArgs = Arg.fromRecord
      { libraryDepFile:
          Arg.argument [ "--output", "-o" ] "Indicates the file that will store the outputted content."
            # Arg.unformat "FILE" Right
            # Arg.default "./ordered-content.txt"
      , finishedDepsFile:
          Arg.argument [ "--deps", "-d" ] "Indicates the file that lists packages that have already been updated on a separate line."
            # Arg.unformat "FILE" Right
            # Arg.default "./finished-dependencies.txt"
      }

-- genSpagoFilesDescription :: String
-- genSpagoFilesDescription =
--   "Generates a `spago.dhall` file named \
--   \`<packageName>.dhall` for each package in the package set. To only output \
--   \such files for specific packages, use the `whitelist` argument. Use this via \
--   \`spago -x <packageName>.dhall build` to install and build \
--   \only that package and all of its dependencies."

-- commandParser :: OA.Parser Command
-- commandParser = OA.subparser $ fold
--   [ OA.command "genLibDeps"
--       $ info (helper <*> parseGenLibDeps)
--       $ progDesc genLibsDescription
--   , OA.command "genSpagoFiles"
--       $ info (helper <*> parseGenSpagoFiles)
--       $ progDesc genSpagoFilesDescription
--   ]

-- parseGenSpagoFiles :: OA.Parser Command
-- parseGenSpagoFiles = map GenSpagoFiles ado
--   directory <- strOption $ fold
--     [ long "dir"
--     , metavar "DIRECTORY"
--     , short 'd'
--     , help "Indicates the directory which will store the outputted \
--            \`<packageName>.dhall` files."
--     , value "./spagoFiles"
--     , showDefault
--     ]
--   whitelist <- option maybeMultiString $ fold
--     [ long "whitelist"
--     , short 'w'
--     , metavar "PACKAGE1,PACKAGE2,...,PACKAGEn"
--     , help "Only produce `spago.dhall` config files for the specified \
--            \list of packages. Packages must be separated by a comma \
--            \with no spaces in-between."
--     ]

--   in { directory, whitelist }

-- maybeMultiString :: ReadM (Maybe (Array String))
-- maybeMultiString = do
--   s <- readerAsk
--   let strArray = filter (not <<< String.null) $ split (Pattern ",") s
--   if Array.null strArray
--     then pure Nothing
--     else pure $ Just $ nub strArray
