module Recipe.Parse
  (parseRecipeFile) where

import Control.Monad (unless)
import Data.Char (isSpace, toLower)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Text.Parsec
import Text.Parsec.String (Parser)
import System.FilePath (takeExtension)

import Recipe.Syntax

-- ----------------------------------------------------------------------------
-- Straightforward string helpers

numberedLines :: String -> [(Int, String)]
numberedLines = zip [1 ..] . lines

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- ----------------------------------------------------------------------------
-- Atoms

portionParser :: Parser String
portionParser = char '[' *> many (noneOf "]") <* char ']'

queryParser :: Parser String
queryParser = many1 (noneOf "[|")

-- ----------------------------------------------------------------------------
-- Parse a particular ingredient line

ingredientParser :: Parser Ingredient
ingredientParser = do
  spaces
  isIdentifier <- option False (True <$ char '!')
  pieces <- many1 (try (Left <$> portionParser) <|> (Right <$> queryParser))
  comment <- optionMaybe (char '|' *> many anyChar)
  spaces
  eof
  let (portions, queryParts) = partitionEithers pieces
      query = trim (concat queryParts)
  unless (not (null query)) (parserFail "ingredient search text cannot be empty")
  pure
    Ingredient
      { query = Text.pack query
      , isIdentifier
      , portions = map (Text.pack . trim) portions
      , description = Text.pack . trim <$> comment
      }

-- ----------------------------------------------------------------------------
-- Parse each (relevant) .recipe line

parseIngredientLines :: [(Int, String)] -> Either String [Ingredient]
parseIngredientLines = traverse parseLine . catMaybes . map eligibleLine
  where
    eligibleLine (lineNumber, source)
      | null trimmed = Nothing
      | "~~" `isPrefixOf` trimmed = Nothing
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

parseRecipeFile :: FilePath -> String -> Either String Recipe
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

fencedContent :: [(Int, String)] -> Either String [(Int, String)]
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
