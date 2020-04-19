{-# LANGUAGE OverloadedStrings #-}

module PlotColors where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Graphics.Plotly.Base as P
import Graphics.Plotly
import Graphics.Plotly.Lucid
import Lens.Micro
import Lucid
import qualified Statistics.Sample.Histogram as S
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Types

-- | Let's do that again, but just take traces.
plotlyChart' :: [Trace] -> T.Text -> Html ()
plotlyChart' traces divName = toHtml $ plotly divName traces
                              & layout . barmode ?~ Stack

-- | Plot multiple texts in one chart,
-- expanding the chart according to the number of texts.
plotlyChartMulti :: [Trace] -> T.Text -> Int -> Html ()
plotlyChartMulti traces divName nTexts = toHtml $ plotly divName traces
                              & layout . barmode ?~ Stack
                              & layout . height ?~ 50 * nTexts
                              & layout . hovermode ?~ Closest

-- | Make traces from color data.
-- We need three traces here. Y is the same in all:
-- the name of the text.
-- X is a list with one value each [x]
-- name is the color name.
mkHBarTraces :: [TextColorStats] -> [Trace]
mkHBarTraces = Prelude.concatMap (\stats -> map (makeTrace (textName stats)) (statsList stats))

-- | Categorize the colors, then plot them as a new bar in our bar plot.
mkHBarParentTraces :: ColorMap -> [TextColorStats] -> [Trace]
mkHBarParentTraces colorMap = Prelude.concatMap makeTraces where
  makeTraces :: TextColorStats -> [Trace]
  makeTraces stats = map (makeTrace textName') colorData' where
    textName' = T.concat [textName stats, "-categories"]
    colorData' = map parentToColor (statsList stats)
    parentToColor :: ColorStat -> ColorStat
    parentToColor colorData = ColorStat { colorWord = parent colorData
                                        , hex = HM.fromList (mapAssoc colorMap) HM.! parent colorData
                                        , parent = "NAN"
                                        , nMatches = nMatches colorData
                                        , locations = locations colorData
                                        }

makeTrace :: TextName -> ColorStat -> Trace
makeTrace textName stats = bars & P.y ?~ [toJSON textName]
                                                & P.x ?~ [toJSON (nMatches stats)]
                                                & name ?~ colorWord stats
                                                & orientation ?~ Horizontal
                                                & marker ?~
                                                (defMarker & markercolor ?~ P.All (toJSON (hex stats)))


-- | Break up the text into N pieces, count the colors in each piece, and then
-- make a Plotly stacked and filled area plot for it. Let's make this JavaScript:
-- @
-- var plotDiv = document.getElementById('plot');
-- var traces = [
-- 	{x: [1,2,3], y: [2,1,4], stackgroup: 'one'},
-- 	{x: [1,2,3], y: [1,1,2], stackgroup: 'one'},
-- 	{x: [1,2,3], y: [3,0,2], stackgroup: 'one'}
-- ];
-- @
-- So we need Xs and Ys.
-- For each trace:
--   * Xs will be chunk indices (e.g. 1-10) and
--   * Ys will be a color and its values (name, color, y-value)
-- since ColorStatsMap = [(TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])])]
mkChunkedTraces :: [TextColorStats] -> -- | Color statistics
                    Int ->  -- | Length of text
                    Int ->  -- | Number of desired chunks
                    [Trace]
mkChunkedTraces allTextStats len nChunks = concatMap makeStat allTextStats where
  makeStat stats = map mkTrace (statsList stats) where
    mkTrace :: ColorStat -> Trace
    mkTrace stat =
      scatter & P.x     ?~ fmap toJSON [1..nChunks]
              & P.y     ?~ fmap toJSON yVals
              & name    ?~ colorWord stat
              & mode    ?~ [Lines]
              & marker ?~ (defMarker & markercolor ?~ P.All (toJSON (hex stat)))
              & stackgroup ?~ "one"
      where
      -- Make Y values, which are the number of times a color appears in a chunk.
      -- Ex: 0 1 2 0 0 0 10 2 0
      yVals = V.toList $ snd (S.histogram nChunks spanVec) :: [Int]
      starts = map (fromIntegral . fst) (locations stat) :: [Double]
      spanVec = V.fromList starts :: V.Vector Double
