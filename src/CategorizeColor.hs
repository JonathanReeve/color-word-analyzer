{-# LANGUAGE OverloadedStrings #-}

module CategorizeColor where

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sortBy, sortOn, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)
import Data.Bifunctor (second)
import Data.Colour.SRGB
import Data.Colour.CIE (luminance, cieLABView)
import Data.Colour.RGBSpace.HSV
import Data.Colour.CIE.Illuminant (d65)

import Types

--  Calculate the distance to each base color, and find the
-- one with the smallest distance.
categorizeColor :: Hex -> ColorMap -> ColorWord
categorizeColor color colorMap = argMin deltas where
  -- Default to white if we can't find the base color
  baseColorMap = [ (baseColor, HM.lookupDefault "#ffffff" baseColor (HM.fromList (mapAssoc colorMap))) | baseColor <- baseColors ]
  -- Make Colour objects for each hex
  baseColours :: [ (ColorWord, Colour Double) ]
  baseColours = map (second readColor) baseColorMap
  deltas :: [ (ColorWord, Double) ]
  deltas = [ second (deltaE76 (readColor color)) baseColor | baseColor <- baseColours ]
  argMin xs = fst $ minimumBy (comparing snd) xs

readColor :: Hex -> Colour Double
readColor hex = case sRGB24reads (T.unpack hex) of
  [] -> error ("Can't read color " ++ (T.unpack hex))
  _ -> fst $ head $ sRGB24reads (T.unpack hex)

-- Given two colors in CIELAB color space, \( {L^*_1},{a^*_1},{b^*_1}) \)
-- and \( {L^*_2},{a^*_2},{b^*_2} \), the CIE76 color difference formula is defined as:
-- \[ \Delta E_{ab}^* = \sqrt{ (L^*_2-L^*_1)^2+(a^*_2-a^*_1)^2 + (b^*_2-b^*_1)^2 } \]
-- https://en.wikipedia.org/wiki/Color_difference
deltaE76 :: Colour Double -> Colour Double -> Double
deltaE76 color1 color2 = sqrt $ (l2-l1)**2 + (a2-a1)**2 + (b2-b1)**2 where
  (l1, a1, b1) = cieLABView d65 color1
  (l2, a2, b2) = cieLABView d65 color2

baseColors :: [ColorWord]
baseColors = ["black", "white", "grey", "red", "orange", "yellow", "green", "blue", "purple"]

-- sortColors :: (Colour Double -> Double) -> [(ColorWord, Hex, Parent, Int, [Span])] -> [(ColorWord, Hex, Parent, Int, [Span])]
-- sortColors selectionFunction colorStats = sortOn sortFunction colorStats where
--   -- convert to HSL and get the hue to sort on.
--   sortFunction (_, hex, _, _, _) = selectionFunction $ readColor hex

sortColors :: (Colour Double -> Double) -> [ColorStat] -> [ColorStat]
sortColors selectionFunction = sortOn (selectionFunction . readColor . fromJust . hex)
