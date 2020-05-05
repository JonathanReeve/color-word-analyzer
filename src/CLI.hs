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

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Lucid
import Data.Hashable

import Graphics.Plotly.Lucid
import Graphics.Plotly

-- My own modules
import qualified ColorMaps as CM
import FindColors
import PlotColors
import AnnotateColors
import Types
import qualified Main

import Data.Xkcd
import Data.Ridgway
import Data.Master
import Data.Pantone
import Data.Jaffer

getColorMapEither cm = case cm of
  "XKCD" -> Right Data.Xkcd.xkcd
  "Ridgway" -> Right Data.Ridgway.ridgway
  "Master" -> Right Data.Master.master
  "Pantone" -> Right Data.Pantone.pantone
  "Jaffer" -> Right Data.Jaffer.jaffer
  "JafferXkcd" -> Right CM.jafferXkcd
  _ -> Left "Invalid color map name"

-- | CLI to annotate colors in text.
-- Usage: runhaskell AnnotateColor my-text-file.txt > out.html
main :: IO ()
main = defaultMain $ do
  programName "Color Word Analyzer"
  programDescription "Text analysis for color words. Experimental."
  colorMap    <- flagParam (FlagLong "colorMap" <>
                           FlagDescription "name of the color map to use, e.g. XKCD")
                (FlagRequired getColorMapEither)
  statsOnly    <- flag (FlagLong "statsOnly" <>
                       FlagDescription "Print JSON instead of HTML.")
  files  <- remainingArguments "FILE(s)"
  action $ \toParam -> do
      putStrLn $ "Using color map: " ++ show (toParam colorMap)
      putStrLn $ "and analyzing files: " ++ show (toParam files)
      let cm = fromJust $ toParam colorMap
      let filesList = toParam files
      if toParam statsOnly then analyzeTextsToJSON cm filesList
      else
        if length filesList == 1 then
          analyzeSingleText cm (head filesList)
        else do
          analyzeMultipleTexts cm filesList
          analyzeTextsToJSON cm filesList

analyzeTextsToJSON colorMap filesList = do
  allStats <- mapM (justStats colorMap) filesList
  let json = toJSON allStats
  let outFileName = show (hash json) ++ ".json"
  BL.writeFile outFileName $ encode json

analyzeMultipleTexts colorMap filesList = do
  traces <- mapM (analyze colorMap) filesList
  let chart = plotlyChartMulti (concat traces) "div1" (length filesList)
  let html = thinScaffold chart
  TIO.putStr $ TL.toStrict $ renderText html

analyzeSingleText colorMap file = do
  fileContents <- TIO.readFile file
  let label = takeBaseName file
  TIO.putStr $ TL.toStrict $ renderText $ Main.doAnalysis fileContents label colorMap

thinScaffold :: Html () -> Html ()
thinScaffold contents =
  html_ $ do
  head_ [] $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    plotlyCDN
  body_ $
    main_ [ class_ "container" ] contents

readAndDecodeFile :: FilePath -> IO T.Text
readAndDecodeFile inFile = do
  rawFile <- B.readFile inFile
  return $ case TE.decodeUtf8' rawFile of
    Left err -> TE.decodeLatin1 rawFile
    Right text -> text

-- analyze :: CM.ColorMap -> FilePath -> [Trace]
analyze :: ColorMap -> FilePath -> IO [Trace]
analyze colorMap file = do
  decoded <- readAndDecodeFile file
  return $ Main.mkTraces decoded (takeBaseName file) colorMap

justStats :: ColorMap -> FilePath -> IO [TextColorStats]
justStats colorMap file = do
  decoded <- readAndDecodeFile file
  return $ Main.mkStats decoded (takeBaseName file) colorMap
