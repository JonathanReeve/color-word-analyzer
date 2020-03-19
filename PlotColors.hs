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

import Types

-- | Just some useful type aliases here

-- plotlyChart :: (ColorStatsMap -> [Trace]) -> [ColorStatsMap] -> T.Text -> Html ()
-- plotlyChart tracesFn colorData divName = mapM_ makeChart colorData where
--   makeChart someData = toHtml $ plotly divName (tracesFn someData)
--                        & layout . margin ?~ thinMargins
--                        & layout . height ?~ 300
--                        & layout . width ?~ 800
--                        & layout . barmode ?~ Stack

-- | Let's do that again, but just take traces.
plotlyChart' :: [Trace] -> T.Text -> Html ()
plotlyChart' traces divName = toHtml $ plotly divName traces
                              & layout . barmode ?~ Stack

-- | Make traces from color data.
-- We need three traces here. Y is the same in all:
-- the name of the text.
-- X is a list with one value each [x]
-- name is the color name.
mkHBarTraces :: ColorStatsMap -> [Trace]
mkHBarTraces = Prelude.concatMap makeTraces where
  makeTraces :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) -> [Trace]
  makeTraces (textName, colorMapName, colorData) = map (makeTrace textName) colorData

mkHBarParentTraces :: ColorMap -> ColorStatsMap -> [Trace]
mkHBarParentTraces colorMap = Prelude.concatMap makeTraces where
  makeTraces :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) -> [Trace]
  makeTraces (textName, colorMapName, colorData) = map (makeTrace textName') colorData' where
    textName' = T.concat [textName, "-cats"]
    colorData' = map parentToColor colorData
    parentToColor (colorWord, hex, parent, n, spans) = (parent, colorMap M.! parent, "NAN", n, spans)

makeTrace :: TextName -> (ColorWord, Hex, Parent, Int, [Span]) -> Trace
makeTrace textName (colorWord, hex, _, n, _) = bars & P.y ?~ [toJSON textName]
                                                & P.x ?~ [toJSON n]
                                                & name ?~ colorWord
                                                & orientation ?~ Horizontal
                                                & marker ?~
                                                (defMarker & markercolor ?~ P.All (toJSON hex))
