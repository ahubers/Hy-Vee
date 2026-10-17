module Recipe.Semantics
  ( eval
  , searchUrl
  ) where

import Data.List (nub)
import Control.Concurrent (threadDelay)
import System.Process (spawnProcess)

import Recipe.Syntax (Ingredient (..), Recipe (..))
import Recipe.Encode

-- ----------------------------------------------------------------------------
-- Constants

baseUrl :: String
baseUrl = "https://www.hy-vee.com/aisles-online/search?search="

firefox :: String 
firefox = "C:\\Users\\ahube\\OneDrive\\Desktop\\firefox.exe"

-- ----------------------------------------------------------------------------
-- Search for each ingredient

searchUrl :: Ingredient -> String
searchUrl = (baseUrl ++) . encodeQuery . query

eval :: Recipe -> IO ()
eval (Recipe ingredients) =
  mapM_ openSearch (nub (map searchUrl ingredients))
  where
    openSearch url = do
      putStrLn ("Opening: " ++ url)
      _ <- spawnProcess firefox [url]
      threadDelay 500000

