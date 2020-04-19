{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics

type Span = (Start, End)
type Start = Int
type End = Int

type ColorOrNot = Either Unmatched (ColorFound, ColorStandardized)
type Unmatched = T.Text
type ColorFound = T.Text
type ColorStandardized = T.Text

type ColorWord = T.Text
type Hex = T.Text
type Parent = T.Text -- Category

type TextName = T.Text
-- type ColorMapName = T.Text

data TextColorStats = TextColorStats { textName :: T.Text
                                     , colorMapName :: T.Text
                                     , statsList :: [ColorStat]
                                     } deriving (Generic, ToJSON, FromJSON)

data ColorStat = ColorStat { colorWord :: T.Text
                           , hex :: T.Text
                           , parent :: T.Text
                           , nMatches :: Double
                           , locations :: [Span]
                           } deriving (Generic, ToJSON, FromJSON)

data ColorMap = ColorMap { mapName :: T.Text
                         , mapAssoc :: [(ColorWord, Hex)]
                         }

instance Show ColorMap where
  show cm = show $ mapName cm
