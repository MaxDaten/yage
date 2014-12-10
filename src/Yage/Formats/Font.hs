{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns       #-}
module Yage.Formats.Font where

import           Yage.Prelude
import           Yage.Lens      hiding ( (.=) )

import           Yage.Font
import           Yage.Image

import qualified Data.Map       as Map

import           Data.Aeson     as JSON
import           Text.Read
import           Filesystem.Path.CurrentOS hiding ( null )


data FontJSON = FontJSON
    { jsonFontMetric        :: !FontMetric
    , jsonFontMap           :: !FilePath
    , jsonCharRegionMap     :: !(Map Char CharData)
    , jsonCharPadding       :: !Int
    }

instance FromJSON GlyphMetrics where
instance ToJSON GlyphMetrics where

instance FromJSON FontGlyph where
instance ToJSON FontGlyph where

instance FromJSON FontDescriptor where
instance ToJSON FontDescriptor where

instance FromJSON FontFace where
instance ToJSON FontFace where

instance FromJSON FontMarkup where
instance ToJSON FontMarkup where

instance FromJSON FontMetric where
instance ToJSON FontMetric where

instance ToJSON FontJSON where
    toJSON FontJSON{..} =
        JSON.object [ "fontMetric"      .= jsonFontMetric
                    , "fontMapFile"     .= fpToText jsonFontMap
                    , "charRegionMap"   .= Map.mapKeys show jsonCharRegionMap
                    , "charPadding"     .= jsonCharPadding
                    ]

instance FromJSON FontJSON where
    parseJSON (JSON.Object v) =
        FontJSON <$> v .: "fontMetric"
                 <*> fmap fpFromText (v .: "fontMapFile")
                 <*> fmap (Map.mapKeys read) (v .: "charRegionMap")
                 <*> v .: "charPadding"
    parseJSON _ = mzero


writeFontTexture :: FilePath -> FontTexture -> IO ()
writeFontTexture filePath fontTexture =
    let yftFile   = replaceExtension filePath "yft"
        pngFile   = replaceExtension filePath "png"
        img       = fontTexture^.fontMap
        fontJson  = FontJSON
          { jsonFontMetric        = fontTexture^.fontMetric
          , jsonFontMap           = filename pngFile
          , jsonCharRegionMap     = fontTexture^.charRegionMap
          , jsonCharPadding       = fontTexture^.charPadding
          }
    in do
        writePng (fpToString pngFile) img
        writeFile yftFile (JSON.encode fontJson)


readFontTexture :: FilePath -> IO FontTexture
readFontTexture yftFile = do
    eFontJson <- JSON.eitherDecode <$> readFile yftFile
    either error toFontTexture eFontJson

    where

    toFontTexture :: FontJSON -> IO FontTexture
    toFontTexture FontJSON{..} =
        let imgFile = directory yftFile </> jsonFontMap
        in do
            eImg <- readImage ( fpToString imgFile )
            either error return $ do
                img <- eImg
                return $ case img of
                    ImageY8 imgY8 -> FontTexture
                        { _fontMetric        = jsonFontMetric
                        , _fontMap           = imgY8
                        , _charRegionMap     = jsonCharRegionMap
                        , _charPadding       = jsonCharPadding
                        }
                    _ -> error "invalid texture format"
