module Recipe.Parse
  (parseRecipeFile) where

import Control.Monad (unless)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.String (Parser)
import System.FilePath (takeExtension)

import Recipe.Prelude
import Recipe.Syntax

-- ----------------------------------------------------------------------------
-- Atoms

commentTokens :: [String]
commentTokens = ["|", "#", "~~"]

isCommentLine :: String -> Bool 
isCommentLine s = or . (map (`isPrefixOf` s)) $ commentTokens

portionParser :: Parser String
portionParser = char '[' *> many (noneOf "]") <* char ']'

itemIdentifierParser :: Parser Text.Text
itemIdentifierParser =
  Text.pack <$> (char '{' *> many1 digit <* char '}')

queryParser :: Parser String
queryParser = many1 (noneOf "[|{}")

data IngredientPart
  = PortionPart String
  | ItemIdentifierPart Text.Text
  | QueryPart String

ingredientPartParser :: Parser IngredientPart
ingredientPartParser =
  (PortionPart <$> try portionParser)
    <|> (ItemIdentifierPart <$> try itemIdentifierParser)
    <|> (QueryPart <$> queryParser)

-- ----------------------------------------------------------------------------
-- Parse a particular ingredient line

ingredientParser :: Parser Ingredient
ingredientParser = do
  spaces
  isKeywordIdentifier <- option False (True <$ char '!')
  pieces <- many1 ingredientPartParser
  comment <- optionMaybe (char '|' *> many anyChar)
  spaces
  eof
  let portions = [portion | PortionPart portion <- pieces]
      itemIdentifiers = [itemId | ItemIdentifierPart itemId <- pieces]
      query = trim (concat [part | QueryPart part <- pieces])
  unless (length itemIdentifiers <= 1) (parserFail "ingredient can have only one item identifier")
  unless (not (isKeywordIdentifier && not (null itemIdentifiers)))
    (parserFail "ingredient cannot combine !keyword and {item-id} identifiers")
  unless (not (null query)) (parserFail "ingredient search text cannot be empty")
  pure
    Ingredient
      { query = Text.pack query
      , identifier =
          case itemIdentifiers of
            itemId : _ -> Just (ItemIdentifier itemId)
            [] | isKeywordIdentifier -> Just KeywordIdentifier
            [] -> Nothing
      , portions = map (Text.pack . trim) portions
      , description = Text.pack . trim <$> comment
      }

-- ----------------------------------------------------------------------------
-- Parse each (relevant) .recipe line

parseIngredientLines :: [(Int, String)] -> Result [Ingredient]
parseIngredientLines = traverse parseLine . catMaybes . map eligibleLine
  where
    eligibleLine (lineNumber, source)
      | null trimmed = Nothing
      | isCommentLine trimmed = Nothing
      | otherwise = Just (lineNumber, source)
      where
        trimmed = trim source

    parseLine (lineNumber, source) =
      case parse ingredientParser ("line " ++ show lineNumber) source of
        Left parseError ->
          Left ("Could not parse recipe line " ++ show lineNumber ++ ": " ++ show parseError)
        Right ingredient -> Right ingredient

-- ----------------------------------------------------------------------------
-- Parse a recipe file and its contents

parseRecipeFile :: FilePath -> String -> Result Recipe
parseRecipeFile path contents
  | lowerExtension == ".recipe" =
      Recipe <$> parseIngredientLines (numberedLines contents)
  | lowerExtension == ".md" && ".recipe.md" `isSuffixOf` lowerPath = do
      fencedLines <- fencedContent (numberedLines contents)
      Recipe <$> parseIngredientLines fencedLines
  | otherwise = Left ("Unsupported recipe file extension: " ++ path)
  where
    lowerPath = map toLower path
    lowerExtension = map toLower (takeExtension path)

-- ----------------------------------------------------------------------------
-- Parse fenced content (delimited by "```")

fencedContent :: [(Int, String)] -> Result [(Int, String)]
fencedContent = go False []
  where
    go inside collected []
      | inside = Left "Unclosed triple-backtick fence"
      | otherwise = Right (reverse collected)
    go inside collected ((lineNumber, source) : remaining)
      | not inside && startsFence trimmed = go True collected remaining
      | inside && trim trimmed == "```" = go False collected remaining
      | inside = go True ((lineNumber, source) : collected) remaining
      | otherwise = go False collected remaining
      where
        trimmed = trim source

    startsFence line = "```" `isPrefixOf` line