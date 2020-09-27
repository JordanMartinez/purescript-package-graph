module Parser where


import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array (foldMap, fromFoldable)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Semigroup.Foldable (fold1)
import Data.String.CodeUnits as SCU
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodeUnits (satisfy, string)
import Text.Parsing.StringParser.Combinators (between, many1, sepBy, sepBy1, (<?>))
import Types (PackageMeta)

parsePackageSetJson :: Parser (HashMap String PackageMeta)
parsePackageSetJson = do
  map fold1 $ betweenCurlyBraces $ parsePackage `sepBy1` comma

-- | `{"<package>":<package meta>}`
parsePackage :: Parser (HashMap String PackageMeta)
parsePackage = do
  package <- quoted parseContent
  void $ colon
  meta <- parsePackageMeta
  pure $ HashMap.singleton package meta

parsePackageMeta :: Parser PackageMeta
parsePackageMeta = do
  betweenCurlyBraces do
    dependencies <- parseDependencies
    void $ comma
    version <- parseField "version" $ quoted parseContent
    void $ comma
    repo <- parseField "repo" $ quoted parseContent
    pure { dependencies, repo, version }

parseVersion :: Parser String
parseVersion = do
  void $ quoted (string "version")
  void $ colon
  quoted parseContent

-- | `"dependencies":["<package name>", "<package name>"]`
parseDependencies :: Parser (Array String)
parseDependencies =
  parseField "dependencies" do
    parseDeps <|> parseNoDeps
  where
    parseDeps =
      betweenBrackets do
        depList <- (quoted parseContent) `sepBy` comma
        pure $ fromFoldable depList

    parseNoDeps = [] <$ string "[]"

-- Combinators

parseField :: forall a. String -> Parser a -> Parser a
parseField fieldName contentParser = do
  void $ quoted (string fieldName)
  void $ colon
  contentParser

quoted :: forall a. Parser a -> Parser a
quoted = between
  (quoteChar <?> "Opening quote not matched")
  (quoteChar <?> "Closing quote not matched")

betweenCurlyBraces :: forall a. Parser a -> Parser a
betweenCurlyBraces = between
  (openCurlyBrace <?> "Opening curly brace not matched")
  (closeCurlyBrace <?> "Closing curly brace not matched")

betweenBrackets :: forall a. Parser a -> Parser a
betweenBrackets = between
  (openBracket <?> "Opening bracket not matched")
  (closeBracket <?> "Closing bracket not matched")

-- Multiple characters

parseContent :: Parser String
parseContent = do
  map (foldMap SCU.singleton) $ many1 $ satisfy (_ /= '"')

-- Single characters

quoteChar :: Parser String
quoteChar = string "\"" <?> "Could not match double-quote character"

colon :: Parser String
colon = string ":" <?> "Could not match colon character"

comma :: Parser String
comma = string "," <?> "Could not match comma character"

openBracket :: Parser String
openBracket = string "[" <?> "Could not match opening bracket character"

closeBracket :: Parser String
closeBracket = string "]" <?> "Could not match closing bracket character"

openCurlyBrace :: Parser String
openCurlyBrace = string "{" <?> "Could not match opening bracket character"

closeCurlyBrace :: Parser String
closeCurlyBrace = string "}" <?> "Could not match closing bracket character"
