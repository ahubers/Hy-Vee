module Recipe.Semantics
  ( eval
  , searchUrl
  , firefox
  ) where

import Control.Monad (foldM)
import Control.Concurrent (threadDelay)
import Data.Char (isSpace)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import System.Process (spawnProcess)

import Recipe.Syntax (Ingredient (..), Recipe (..))
import Recipe.Encode

-- ----------------------------------------------------------------------------
-- Constants

baseUrl :: String
baseUrl = "https://www.hy-vee.com/aisles-online/search?search="

firefox :: String 
firefox = "C:\\Program Files\\Mozilla Firefox\\firefox.exe"

-- ----------------------------------------------------------------------------
-- Search for each ingredient

searchUrl :: Ingredient -> String
searchUrl = (baseUrl ++) . encodeQuery . query

itemDatabasePath :: FilePath
itemDatabasePath = ".items.recipe-db"

parseItemDatabase :: String -> Either String (Map Text.Text Text.Text)
parseItemDatabase = foldM parseEntry Map.empty . zip [1 :: Int ..] . lines
  where
    parseEntry database (lineNumber, source)
      | all isSpace source = Right database
      | otherwise =
          let (key, suffix) = break (== ':') source
              normalizedKey = Text.toCaseFold (Text.pack (trim key))
              itemId = drop 1 suffix
           in if null suffix || Text.null normalizedKey || not (all isDigit itemId)
                then Left (itemDatabasePath ++ ": invalid entry on line " ++ show lineNumber ++ "; expected keyword:item-id")
                else
                  if Map.member normalizedKey database
                    then Left (itemDatabasePath ++ ": duplicate keyword on line " ++ show lineNumber)
                    else Right (Map.insert normalizedKey (Text.pack itemId) database)

    isDigit character = character >= '0' && character <= '9'
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

itemUrl :: Map Text.Text Text.Text -> Ingredient -> String
itemUrl database ingredient
  | isIdentifier ingredient =
      case Map.lookup (Text.toCaseFold (query ingredient)) database of
        Just itemId -> productBaseUrl ++ Text.unpack itemId
        Nothing -> searchUrl ingredient
  | otherwise = searchUrl ingredient
  where
    productBaseUrl = "https://www.hy-vee.com/aisles-online/p/"

eval :: Recipe -> IO ()
eval (Recipe ingredients) = do
  databaseContents <- readFile itemDatabasePath
  database <-
    either (ioError . userError) pure (parseItemDatabase databaseContents)
  mapM_ openSearch (nub (map (itemUrl database) ingredients))
  where
    openSearch url = do
      putStrLn ("Opening: " ++ url)
      _ <- spawnProcess firefox ["-new-tab", url]
      threadDelay 500000
