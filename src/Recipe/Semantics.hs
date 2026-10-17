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

import Recipe.Prelude
import Recipe.Syntax (Identifier (..), Ingredient (..), Recipe (..))
import Recipe.Encode
import Recipe.Database

-- ----------------------------------------------------------------------------
-- Constants

baseUrl :: Url
baseUrl = "https://www.hy-vee.com/aisles-online/search?search="

productBaseUrl :: Url  
productBaseUrl = "https://www.hy-vee.com/aisles-online/p/" 

firefox :: FilePath
firefox = "C:\\Program Files\\Mozilla Firefox\\firefox.exe"

itemDatabasePath :: FilePath
itemDatabasePath = ".items.recipe-db"

-- ----------------------------------------------------------------------------
-- Search for each ingredient

searchUrl :: Ingredient -> String
searchUrl = (baseUrl ++) . encodeQuery . query

itemUrl :: Database -> Ingredient -> String
itemUrl database ingredient =
  case getId database ingredient of 
    Just i -> productBaseUrl ++ i ++ "/"
    _      -> searchUrl ingredient

eval :: Recipe -> IO ()
eval (Recipe ingredients) = do
  databaseContents <- readFile itemDatabasePath
  database <-
    either (ioError . userError) pure (parseItemDatabase itemDatabasePath databaseContents)
  mapM_ openSearch (nub (map (itemUrl database) ingredients))
  where
    openSearch url = do
      putStrLn ("Opening: " ++ url)
      _ <- spawnProcess firefox ["-new-tab", url]
      threadDelay 500000
