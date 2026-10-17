module Recipe.Database where 

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (pack , unpack , toCaseFold)

import Recipe.Prelude
import Recipe.Syntax

-- ----------------------------------------------------------------------------
-- Item Database

type Database = Map Text.Text Text.Text

parseItemDatabase :: FilePath -> String -> Result Database
parseItemDatabase itemDatabasePath = foldM parseEntry Map.empty . zip [1 :: Int ..] . lines
  where
    parseEntry database (lineNumber, source)
      | all isSpace source = Right database
      | otherwise =
          let (key, suffix) = break (== ':') source
              normalizedKey = toCaseFold (pack (trim key))
              itemId = drop 1 suffix
           in if null suffix || Text.null normalizedKey || null itemId || not (all isDigit itemId)
                then Left (itemDatabasePath ++ ": invalid entry on line " ++ show lineNumber ++ "; expected keyword:item-id")
                else
                  if Map.member normalizedKey database
                    then Left (itemDatabasePath ++ ": duplicate keyword on line " ++ show lineNumber)
                    else Right (Map.insert normalizedKey (pack itemId) database)
    isDigit character = character >= '0' && character <= '9'
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


getId :: Database -> Ingredient -> Maybe String 
getId database ingredient = 
  case identifier ingredient of
    Just (ItemIdentifier itemId) -> Just (unpack itemId)
    Just KeywordIdentifier -> 
      fmap unpack (Map.lookup 
        (toCaseFold (query ingredient)) database)
    _ -> Nothing