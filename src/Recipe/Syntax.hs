module Recipe.Syntax
  ( Identifier (..)
  , Ingredient (..)
  , Recipe (..)
  ) where

import Data.List (intercalate)
import Data.Text (Text, unpack)


-- ----------------------------------------------------------------------------
-- Ingredients

data Identifier
  = KeywordIdentifier
  | ItemIdentifier Text
  deriving (Eq)

data Ingredient = Ingredient
  { query       :: Text
  , identifier  :: Maybe Identifier
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
  show (Ingredient { query , identifier , portions , description }) =
    (case identifier of
       Just KeywordIdentifier -> "!"
       Just (ItemIdentifier _) -> ""
       Nothing -> "")
      ++ unpack query
      ++ showPortions portions
      ++ (case identifier of
            Just (ItemIdentifier itemId) -> " {" ++ unpack itemId ++ "}"
            _ -> "")
      ++ showDescription description
    
-- ----------------------------------------------------------------------------
-- Recipes

newtype Recipe = Recipe
  { ingredients :: [Ingredient]
  }
  deriving (Eq)

instance Show Recipe where 
  show (Recipe { ingredients }) = concatMap (++ "\n") . fmap show $ ingredients
