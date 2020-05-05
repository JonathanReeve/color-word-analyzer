
module FindColors where

import qualified Data.Text as T
import Data.Attoparsec.Text as AT
-- import Replace.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.Char (isSpace, isPunctuation)

import Types

-- * Parsing the colors

-- | Takes a list of words like "light green blue" and makes a
-- parser which will find "light green blue" and also "light green-blue",
-- "light green\nblue" and so on.
wordListParser :: [T.Text] -> Parser T.Text
wordListParser [w] = asciiCI w -- One word case
wordListParser (w:ws) = do  -- Multi-word case
  a <- asciiCI w    -- single word
  b <- space <|> char '-' -- followed by a separator
  c <- wordListParser ws          -- and more words
  -- Parse to space-separated.
  return (a `T.append` T.singleton ' ' `T.append` c) -- singleton :: Char -> Text
wordListParser [] = asciiCI T.empty

-- | Parse word boundaries.
-- This reads really well, but is disabled because it was too slow.
-- wordBoundary :: Parser Char
-- wordBoundary = space <|> satisfy isPunctuation

-- wordBoundary :: Parser ()
-- wordBoundary = skipWhile (\c -> c `elem` " \n\t\r!@#$%^&*()-=_+[]{}|:;'\"<«»–—?.,")

wordBoundary :: Parser ()
wordBoundary = skipSpace <|> skipWhile isPunctuation

-- | Don't just parse the word, parse it with word boundaries
-- on either side.
withBoundaries :: Parser T.Text -> Parser T.Text
withBoundaries word = wordBoundary *> word <* wordBoundary

-- | Make one big parser out of our color map, and the expressions
-- generated from wordListParser.
colorParser :: [(ColorWord, Hex)] -> Parser T.Text
colorParser colorMap = choice $ map withBoundaries parsers where
  colorMapWords = map (T.words . fst) colorMap
  colorMapFiltered = filter (/= []) colorMapWords
  parsers = map wordListParser colorMapFiltered
