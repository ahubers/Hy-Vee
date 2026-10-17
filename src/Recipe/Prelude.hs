module Recipe.Prelude where

import Data.Char (isSpace)

-- ----------------------------------------------------------------------------
-- Straightforward string helpers

numberedLines :: String -> [(Int, String)]
numberedLines = zip [1 ..] . lines

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- ----------------------------------------------------------------------------
-- Messages and Results

type Message = String
type Result a = Either Message a
type Url = String 