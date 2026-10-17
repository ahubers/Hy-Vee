module Recipe.Syntax
  ( Ingredient (..)
  , Recipe (..)
  ) where

import Data.List (intercalate)
import Data.Text (Text, unpack)


-- ----------------------------------------------------------------------------
-- Ingredients

data Ingredient = Ingredient
  { query       :: Text
  , portions    :: [Text]
  , description :: Maybe Text
  } deriving (Eq)

enclose :: String -> String -> String -> String
enclose l r x = l ++ x ++ r 

brackets :: String -> String
brackets = enclose "[" "]"


showPortions :: [Text] -> String 
showPortions portions
  | null portions = "" 
  | otherwise     = brackets $ intercalate "," (unpack <$> portions)

showDescription :: Maybe Text -> String
showDescription (Just t) = " | " ++ unpack t
showDescription _ = ""

instance Show Ingredient where 
  show (Ingredient { query , portions , description }) = 
    unpack query ++ showPortions portions ++ showDescription description
    
-- ----------------------------------------------------------------------------
-- Recipes

newtype Recipe = Recipe
  { ingredients :: [Ingredient]
  }
  deriving (Eq)

instance Show Recipe where 
  show (Recipe { ingredients }) = concatMap (++ "\n") . fmap show $ ingredients

