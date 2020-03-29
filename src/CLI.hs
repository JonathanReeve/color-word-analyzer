{-# LANGUAGE OverloadedStrings #-}

-- AnnotateColor: a module and CLI for
-- extracting color word data from text.
-- Part of my dissertation,
-- https://github.com/JonathanReeve/dissertation
-- All code licensed under the GPLv3.

module CLI where

import Codec.Text.Detect (detectEncoding)
import System.FilePath
import Console.Options

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL

-- My own modules
import qualified ColorMaps as CM
import FindColors
import PlotColors
import AnnotateColors
import Types
import qualified Main

colorMapParser :: ValueParser CM.ColorMap
colorMapParser cmOption = case (CM.getColorMap cmOption) of
                            CM.ColorMap name assoc -> Right (CM.getColorMap cmOption)
                            _ -> Left "That's not a valid color map."


-- | CLI to annotate colors in text.
-- Usage: runhaskell AnnotateColor my-text-file.txt > out.html
main :: IO ()
main = defaultMain $ do
  programName "Color Word Analyzer"
  programDescription "Text analysis for color words. Experimental."
  colorMap    <- flagParam (FlagLong "colorMap") (FlagRequired colorMapParser)
  files  <- remainingArguments "FILE(s)"
  action $ \toParam -> do
      putStrLn $ "Using color map: " ++ show (toParam colorMap)
      putStrLn $ "and analyzing files: " ++ show (toParam files)

   -- -- inFile <- TIO.readFile fileName
   -- rawByteStr <- B.readFile fileName

   -- -- Try to decode Utf-8 first, but if not, try Latin1.
   -- let inFile = case TE.decodeUtf8' rawByteStr of
   --                Left err -> TE.decodeLatin1 rawByteStr
   --                Right text -> text

   -- let label = takeBaseName fileName

   -- let cm = CM.xkcd
   -- colorMap <- CM.assoc cm
   -- let html = doAnalysis inFile label colorMap (CM.name cm)

   -- -- Write to a file.
   -- let outFileName = label ++ "-out.html"
   -- renderToFile outFileName html

   -- -- Output just the data.
   -- -- TIO.putStrLn . TE.decodeUtf8 . BL.toStrict . encode $ stats
