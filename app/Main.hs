module Main (main) where

import Control.Monad (forM)
import Recipe.Semantics (eval , firefox)
import Recipe.Syntax (Recipe (..))
import Recipe.Parse (parseRecipeFile)
import System.Environment (getArgs)
import System.Exit (die)

-- ----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  arguments <- getArgs
  let paths = if null arguments then ["recipes/staples.recipe.md"] else arguments
  recipes <- forM paths loadRecipe
  eval (Recipe (concatMap ingredients recipes))
  where
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