module Main (main) where

import Control.Monad (forM)
import Data.Char (toLower)
import Recipe.Semantics (eval , firefox)
import Recipe.Syntax (Recipe (..))
import Recipe.Parse (parseRecipeFile)
import Recipe.Prelude (trim)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (isAbsolute, takeExtension, (</>))

-- ----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  arguments <- getArgs
  let paths = if null arguments then ["recipes/staples.recipe.md"] else arguments
  recipePaths <- concat <$> forM paths expandPath
  recipes <- forM recipePaths loadRecipe
  eval (Recipe (concatMap ingredients recipes))
  where
    expandPath path
      | map toLower (takeExtension path) == ".order" = do
          contents <- readFile path
          pure
            [ if isAbsolute recipePath then recipePath else "recipes" </> recipePath
            | recipePath <- map trim (lines contents)
            , not (null (trim recipePath))
            ]
      | otherwise = pure [path]

    loadRecipe path = do
      contents <- readFile path
      case parseRecipeFile path contents of
        Left parseError -> die (path ++ ": " ++ parseError)
        Right recipe -> pure recipe


testParse :: String -> IO () 
testParse file = do 
  contents <- readFile file 
  case parseRecipeFile file contents of
    Left parseError -> die parseError
    Right recipe -> putStrLn (show recipe)
  return ()