{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                        (join, forM_)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Applicative                  ((<$>))
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           System.Environment                   (lookupEnv)
import           Text.Read                            (readMaybe)
import           Web.Scotty

import Data.Maybe

import Network.Wai.Parse
import Network.HTTP.Types.Status

import System.FilePath ((</>), takeBaseName)

import qualified Clay as C
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import Lucid

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

import Graphics.Plotly.Lucid (plotlyCDN)
import Graphics.Plotly

-- import Main

import qualified ColorMaps as CM
import FindColors
import AnnotateColors
import PlotColors
import Types

scaffold :: Html () -> Html ()
scaffold contents =
  html_ $ do
    head_ [] $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      style_ [type_ "text/css"] $ C.render css
      plotlyCDN
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre.min.css" ]
      link_ [ rel_ "stylesheet",
              href_ "https://unpkg.com/spectre.css/dist/spectre-icons.min.css" ]
    body_ $ do
      navBar
      main_ [ class_ "container" ] contents
      scripts


homepage :: Html ()
homepage = scaffold $ do
  div_ [ class_ "hero bg-gray" ] $
    div_ [ class_ "hero-body" ] $ do
    h1_ "Color Word Analyzer"
    p_ "This tool searches your English-language text for color words, or passages with colorful content."
  uploadForm

uploadForm :: Html ()
uploadForm =
  form_ [method_ "post", enctype_ "multipart/form-data", action_ "/upload" ] $
  div_ [ class_ "form-group" ] $ do
  span_ [ style_ "large" ] "1"
  label_ [for_ "colorMapSelector"] "Choose a word-to-color mapping."
  div_ [ class_ "form-group" ] $
    select_ [id_ "colorMapSelector", name_ "colorMap", class_ "form-select" ] $
    forM_ CM.colorMaps (\colorMap -> option_ [value_ (mapName colorMap)]
                      (toHtml (mapName colorMap)))
  label_ [ for_ "fileSelector" ] "Choose a file to upload."
  input_ [ class_ "form-input", type_ "file",
            name_ "uploadedFile", id_ "fileSelector" ]
  input_ [ class_ "btn btn-primary", type_ "submit" ]

navBar :: Html ()
navBar = header_ [ class_ "navbar container bg-primary text-secondary" ] $ do
  section_ [ class_ "navbar-section" ] "Color Word Imaginer"
  section_ [ class_ "navbar-section" ] $ do
    a_ [ class_ "btn btn-link text-secondary" ] ("about" :: Html ())
    a_ [ class_ "btn btn-link text-secondary" ] ("upload" :: Html ())

scripts :: Html ()
scripts = mapM_ (\src -> with (script_ "") [ src_ src ]) [ "" ]

-- | The styling for the result web page
css :: C.Css
css = do
  "main, header" C.?
    C.maxWidth (C.pct 80)
  "div.annotated" C.? do
    C.backgroundColor "#555"
    C.color "#ddd"


readInfile :: BS.ByteString -> T.Text
readInfile fc = case TE.decodeUtf8' fc of
               Left err -> TE.decodeLatin1 fc
               Right text -> text

doAnalysis :: T.Text ->
             -- ^ Input file
             String ->
             -- ^ Input file label
             ColorMap ->
             -- ^ Color mapping
             Html ()
             -- ^ Resulting HTML
doAnalysis inFile label colorMap = do
  let colorMapAssoc = mapAssoc colorMap
      parsed = findReplace (colorParser colorMapAssoc) inFile
      zipData = map getZipData (zip (getLocations parsed) parsed)
      onlyMatches = catMaybes zipData
      colorMapLabel = mapName colorMap
      stats = makeStats (T.pack label) (listToMap onlyMatches) colorMap
      textLength = T.length inFile
      adjustedByLength = adjustStatsByLength stats textLength
  mkHtml colorMap [adjustedByLength] parsed textLength

-- adjustStatsByLength :: (TextName, ColorMapName, [(ColorWord, Hex, Parent, Int, [Span])]) ->
--                       Int ->
--                       -- ^ Text length
--                       (TextName, ColorMapName, [(ColorWord, Hex, Parent, Double, [Span])])
-- adjustStatsByLength (textName, colorMapName, stats) len = (textName, colorMapName, map adjustStat stats) where
--   adjustStat :: (ColorWord, Hex, Parent, Int, [Span]) -> (ColorWord, Hex, Parent, Double, [Span])
--   adjustStat (cw, hex, par, n, spans) = (cw, hex, par, adjustedLen, spans)
--   adjustedLen = n / len :: Double

adjustStatsByLength :: TextColorStats ->
                      Int ->
                      -- ^ Text length
                      TextColorStats
adjustStatsByLength stats len = stats { statsList = map adjustStat (statsList stats)
                                      } where
  adjustStat :: ColorStat -> ColorStat
  adjustStat stats  = stats { nMatches = (nMatches stats / toEnum len) :: Double }

-- | Only make the first chart, and don't scaffold
mkTraces :: T.Text ->
             -- ^ Input file
             String ->
             -- ^ Input file label
             ColorMap ->
             -- ^ Color mapping
             [Trace]
             -- ^ Resulting HTML
mkTraces inFile label colorMap = do
  let parsed = findReplace (colorParser (mapAssoc colorMap)) inFile
      zipData = map getZipData (zip (getLocations parsed) parsed)
      onlyMatches = catMaybes zipData
      stats = makeStats (T.pack label) (listToMap onlyMatches) colorMap
      textLength = T.length inFile
      adjustedByLength = adjustStatsByLength stats textLength
  mkHBarTraces [adjustedByLength] -- ++ (mkHBarParentTraces colorMapMap [stats])
  -- plotlyChart' barTraces (T.pack label)

-- | Only make the first chart, and don't scaffold
mkStats :: T.Text ->
          -- ^ Input file contents
          String ->
          -- ^ Input file label
          ColorMap ->
          -- ^ Color mapping
          [TextColorStats]
          -- ^ Resulting HTML
mkStats inFile label colorMap = do
  let colorMapMap = HM.fromList $ mapAssoc colorMap
      parsed = findReplace (colorParser (mapAssoc colorMap)) inFile
      zipData = map getZipData (zip (getLocations parsed) parsed)
      onlyMatches = catMaybes zipData
      stats = makeStats (T.pack label) (listToMap onlyMatches) colorMap
      textLength = T.length inFile
      adjustedByLength = adjustStatsByLength stats textLength
  return adjustedByLength

mkHtml :: Types.ColorMap -> [TextColorStats] -> [ColorOrNot] -> Int -> Html ()
mkHtml colorMap stats parsed len = scaffold $ do
    h1_ [] "Color Words in Aggregate"
    let barTraces = mkHBarTraces stats ++ mkHBarParentTraces colorMap stats
    plotlyChart' barTraces "div1"
    h1_ [] "Color Words in Narrative Timeseries"
    let lineTraces = mkChunkedTraces stats len 10
    plotlyChart' lineTraces "div2"
    h1_ [] "Annotated Text"
    let annotated = annotate colorMap parsed
    div_ [ class_ "annotated" ] $ toHtmlRaw annotated

main :: IO ()
main = do
  port <- fromMaybe 3000
    . join
    . fmap readMaybe <$> lookupEnv "PORT"
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "uploads/") -- for favicon.ico
    middleware logStdoutDev

    get "/" $
      html $ renderText homepage

    post "/upload" $ do
      cm <- param ("colorMap" :: TL.Text) :: ActionM TL.Text
      -- Send 202 "accepted" code as temporary
      status status202
      fs <- files
      let fs' = head [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
      -- liftIO $ print fs'
      -- write the files to disk, so they will be served by the static middleware
      let (_, fn, fc) = fs'
      -- liftIO $ B.writeFile ("uploads" </> fn) fc
      let colorMap = CM.getColorMap cm
      let contents = readInfile $ B.toStrict fc
      let label = takeBaseName fn
      html $ renderText $ doAnalysis contents label colorMap
