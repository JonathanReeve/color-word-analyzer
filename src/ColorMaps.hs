{-# LANGUAGE OverloadedStrings #-}

module ColorMaps where
import qualified Data.HashMap.Strict as HM
import Data.Ridgway
import Data.Xkcd
import Data.Master
import Data.Jaffer
import Data.Pantone
import Data.List

import CategorizeColor
import Types

-- | Take a color map containing things like ("nile blue", "#4E5180")
--   and return ("nile", "#4E5180"), ("blue", "#4E5180")
-- extendMap :: [(ColorWord, Hex)] -> [(ColorWord, Hex)]
-- extendMap colorMap = concatMap split colorMap where
--   split :: (ColorWord, Hex) -> [(ColorWord, Hex)]
--   split (cw, hex) = map (\word -> (word, hex)) (T.words cw)

-- -- | An extended Ridgway, with base colors from XKCD,
-- -- since Ridgway bizarrely doesn't have base colors.
-- ridgwayXkcdMap = do
--   let rwMap = HM.fromList . extendMap <$> mapAssoc ridgway
--       xkcdMap = HM.fromList <$> mapAssoc Data.Xkcd.xkcd
--       xkcdBase = HM.fromList [ (baseColor, (M.findWithDefault "#ffffff" baseColor xkcdMap))
--                              | baseColor <- baseColors ]
--   return (HM.union xkcdBase rwMap)
--   -- return $ [ M.insert (fst item) (snd item) rwMap | item <- xkcdBase ]
--   -- return $ foldl (\item -> M.insert (fst item) (snd item) rwMap) xkcdBase

-- ridgwayExtendedXkcd = ColorMap { mapName = "RidgwayExtendedXKCD"
--                                , mapAssoc = HM.toList <$> ridgwayXkcdMap
--                                }

xkcd = Data.Xkcd.xkcd

jafferXkcd :: ColorMap
jafferXkcd = ColorMap { mapName = "Jaffer+XKCD"
                      , mapAssoc = mapAssoc Data.Jaffer.jaffer ++ mapAssoc Data.Xkcd.xkcd
                      }

colorMaps = [ Data.Xkcd.xkcd
            , Data.Ridgway.ridgway
            , Data.Master.master
            , Data.Jaffer.jaffer
            , Data.Pantone.pantone
            , jafferXkcd
            ]

getColorMap cm = case cm of
  "XKCD" -> Data.Xkcd.xkcd
  "Ridgway" -> Data.Ridgway.ridgway
  "Master" -> Data.Master.master
  "Jaffer" -> Data.Jaffer.jaffer
  "Pantone" -> Data.Pantone.pantone
  "JafferXkcd" -> jafferXkcd
  -- "RidgwayExtendedXKCD" -> ridgwayExtendedXkcd

-- Make a hash map of color words and their parents,
-- so that we can speed up the categorization process.
categoryMap :: ColorMap -> HM.HashMap ColorWord Parent
categoryMap cm = HM.fromList [ (fst pair, categorizeColor (snd pair) cm) | pair <- mapAssoc cm ]
