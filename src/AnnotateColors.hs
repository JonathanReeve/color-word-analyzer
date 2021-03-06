{-# LANGUAGE OverloadedStrings #-}

-- import Main
module AnnotateColors where

-- import Main
import Data.Maybe (fromMaybe, catMaybes)
import Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as TL
import Data.Attoparsec.Text as AT
import Replace.Attoparsec.Text
import Lucid
import Data.Either
import Data.Colour.SRGB
import Data.Colour.CIE

import Types
import CategorizeColor
import FindColors
import ColorMaps

-- | Annotate color words in text, using HTML
annotate :: ColorMap -> [ColorOrNot] -> T.Text
annotate colorMapObj results = T.concat $ Prelude.map processBlock results where
  colorMap = HM.fromList $ mapAssoc colorMapObj
  processBlock :: ColorOrNot -> T.Text
  processBlock block = case block of
    Left txt -> txt
    -- textFormat is the way the color expression is formatted in the text;
    -- stdFormat is the way it is formatted in the standardized way that it appears in the
    -- color map.
    Right (textFormat, stdFormat) ->
      -- Lowercase it first.
      let stdFormatLower = T.toLower stdFormat in
        TL.toStrict $ makeSpan textFormat $ HM.lookupDefault (T.concat ["CANTFIND", stdFormat]) stdFormatLower colorMap



-- | Takes the parser output and makes spans
-- (start, end) for their locations
getLocations :: [ColorOrNot] -> [Span]
getLocations xs = zip <*> tail $ 0:scanl1 (+) (getLengths xs) where
  getLengths :: [ColorOrNot] -> [Int]
  getLengths = fmap getLength 
  getLength x = case x of
    Left text -> T.length text
    Right (txtFormat, stdFormat) -> T.length txtFormat

-- | Actually do the replacement in the text.
findReplace :: Parser T.Text -> T.Text -> [ColorOrNot]
findReplace parser sourceText = fromRight [] $ parseOnly (findAllCap parser) sourceText

-- | Makes HTML from a color word and hex pair.
-- I.e. "red" -> "<span class="color" style="color: #ff0000">"
makeSpan :: T.Text -> T.Text -> TL.Text
makeSpan colorWord hex = TL.concat [" ", t, " "] where
  t = renderText $ span_ attrs (toHtml colorWord)
  attrs = [ class_ "color", style_ (T.concat ["color: ", hex])::Attribute ]

-- | Utility for making printable data sets of the color name locations
-- so that they can be used in analysis later.
getZipData :: (Span, ColorOrNot) -> Maybe (Span, ColorFound, ColorStandardized)
getZipData (locs, parsed) = case parsed of
  Left _ -> Nothing
  Right (txtFormat, stdFormat) -> Just (locs, txtFormat, stdFormat)

makeStats :: TextName -> M.Map ColorWord [Span] -> ColorMap -> TextColorStats
makeStats fileName locs colorMap = TextColorStats { textName = fileName
                                                  , colorMapName = mapName colorMap
                                                  , statsList = stats } where
  -- TODO: add more sort functions than just luminance.
  stats = sortColors luminance $ mapMaybe makeStat (M.toList locs)
  parentMap = ColorMaps.categoryMap colorMap
  makeStat (colorWord, spans) = let
    hex = HM.lookup colorWord (HM.fromList (mapAssoc colorMap))
    parent = HM.lookup colorWord parentMap
    in
      if isJust hex && isJust parent
      then Just ColorStat { colorWord = colorWord
                          , hex = hex
                          , parent = parent
                          , nMatches = toEnum (length spans) :: Double
                          , locations = spans}
      else Nothing

-- | Utility to convert a list [("a", 2), ("a", 3), ("b", 2)] to a Map
-- like [("a", [2, 3]), "b", [2])]
listToMap :: [(Span, b, T.Text)] -> M.Map ColorWord [Span]
listToMap l = M.fromListWith (++) [(T.toLower stdFormat, [(loc1, loc2)]) |
                                   ((loc1, loc2), txtFormat, stdFormat) <- l]

