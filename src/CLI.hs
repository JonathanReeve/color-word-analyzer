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
import Data.Maybe
import Lucid

import Graphics.Plotly.Lucid
import Graphics.Plotly

-- My own modules
import qualified ColorMaps as CM
import FindColors
import PlotColors
import AnnotateColors
import Types
import qualified Main


getColorMapEither cm = case cm of
  "XKCD" -> Right CM.xkcd
  "Ridgway" -> Right CM.ridgway
  "RidgwayExtendedXKCD" -> Right CM.ridgwayExtendedXkcd
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
  files  <- remainingArguments "FILE(s)"
  action $ \toParam -> do
      putStrLn $ "Using color map: " ++ show (toParam colorMap)
      putStrLn $ "and analyzing files: " ++ show (toParam files)
      let firstFile = head $ toParam files
      let cm = fromJust $ toParam colorMap
      cmm <- CM.assoc cm
      let cmLabel = CM.name cm
      let filesList = toParam files
      traces <- mapM (analyze cmm cmLabel) filesList
      let chart = plotlyChart' (concat traces) "div1"
      let html = thinScaffold chart
      TIO.putStr $ TL.toStrict $ renderText html
      -- TIO.putStr $ TL.toStrict $ TL.concat $ map renderText htmls

thinScaffold :: Html () -> Html ()
thinScaffold contents =
  html_ $ do
  head_ [] $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    plotlyCDN
  body_ $
    main_ [ class_ "container" ] contents

-- analyze :: CM.ColorMap -> FilePath -> [Trace]
analyze colorMap cmLabel file = do
  rawFile <- B.readFile file
  let decoded = case TE.decodeUtf8' rawFile of
                  Left err -> TE.decodeLatin1 rawFile
                  Right text -> text
  let label = takeBaseName file
  return $ Main.mkTraces decoded label colorMap cmLabel
