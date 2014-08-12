{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Yage.Font.FontTexture
    ( module Yage.Font.FontTexture
    , module Rectangle
    , module FTExport
    ) where

import Yage.Prelude hiding (Text)
import Yage.Lens

import System.IO.Unsafe (unsafePerformIO)

import Yage.Data.List (piz)
import Data.Map hiding (map, null)
import Yage.Image.BilinearInterpolation

import Graphics.Font as FT
import Graphics.Font as FTExport ( FontLoadMode(..)
                                 , FontDescriptor(..)
                                 , FontLibrary
                                 , FontGlyph(..), GlyphMetrics(..), FontFace(..)
                                 , makeLibrary, freeLibrary, withNewLibrary
                                 , loadFont
                                 )

import Yage.Images
import Yage.Image.SDF

import Yage.Texture.TextureAtlas
import qualified Yage.Texture.Atlas.Builder as Builder

import Yage.Geometry.D2.Rectangle as Rectangle


---------------------------------------------------------------------------------------------------

type CharData = (FontGlyph, TextureRegion)
data FontMarkup = FontMarkup
    { _horizontalSpacing :: Double
    , _verticalSpacing   :: Double
    } deriving ( Show, Eq, Generic )

makeLenses ''FontMarkup

data FontMetric = FontMetric
    { _fontName          :: !String
    , _fontDescriptor    :: !FontDescriptor
    , _fontMarkup        :: !FontMarkup
    , _fontFace          :: !FontFace
    } deriving ( Show, Eq, Generic )

makeLenses ''FontMetric

data FontTexture = FontTexture
    { _fontMetric        :: !FontMetric
    , _fontMap           :: !(Image Pixel8)
    -- ^ the image as a backend for the font
    , _charRegionMap     :: !(Map Char CharData)
    , _charPadding       :: !Int
    } deriving ( Typeable, Generic )

makeLenses ''FontTexture


makeFontTexture :: Font -> FontMarkup -> TextureAtlas Char (Image Pixel8) -> FontTexture
makeFontTexture font markup textureAtlas =
    let glyphM   = charMap font
        regionM  = textureAtlas^.atlasRegions
    in FontTexture
        { _charRegionMap  = unionRegionsWithGlyphs regionM glyphM
        , _charPadding    = textureAtlas^.atlasPadding
        , _fontMap        = textureAtlas^.atlasImage
        , _fontMetric     = FontMetric
                            { _fontName       = FT.fontName font
                            , _fontDescriptor = fontDescr font
                            , _fontMarkup     = markup
                            , _fontFace       = snd $ FT.fontFace font
                            }
        }
    where
        unionRegionsWithGlyphs = intersectionWith (flip (,))


generateFontBitmapTexture
    :: Font ->
       FontMarkup ->
       FontLoadMode ->
       [Char] ->
       Builder.AtlasSettings Pixel8 ->
       Either [(Char, Builder.AtlasError Char)] FontTexture
generateFontBitmapTexture font markup mode chars settings =
    let imgs    = sortBy descArea $ map (unsafePerformIO . generateCharImg font mode) chars `piz` chars
    in makeFontTexture font markup <$> Builder.newTextureAtlas settings imgs

    where

    descArea (_, img1) (_, img2) = descending imageByAreaCompare img1 img2


sdfFontTexture :: Int -> FontTexture -> FontTexture
sdfFontTexture spread fontTex =
    fontTex & fontMap %~ (`signedDistanceField` spread)


downscaleFontTexture :: Int -> FontTexture -> FontTexture
downscaleFontTexture 0 _fontTex = error "invalid scale factor: 0"
downscaleFontTexture 1 fontTex  = fontTex
downscaleFontTexture scalefactor fontTex =
    fontTex & fontMap       %~ scaleImg
            & charRegionMap %~ over (mapped._2) adjustRegion

    where

    scaleImg img =
        let (newWidth, newHeight) = ( imageWidth img `div` scalefactor
                                    , imageHeight img `div` scalefactor
                                    )
        in bilinear newWidth newHeight img

    adjustRegion :: TextureRegion -> TextureRegion
    adjustRegion = over mapped (`scale` scalefactor)

    scale x y = ceiling $ fromIntegral x / fromIntegral y



instance Show FontTexture where
    show FontTexture{..} =
        unpack $ format (
            "FontTexture { fontName: {}, loadedChars: {}, padding: {}, image: {}, " ++
            "descriptor: {}, markup: {}, face: {}}"
            ) ( Shown _fontMetric
              , Shown $ size _charRegionMap
              , Shown _charPadding
              , Shown ( imageWidth _fontMap, imageHeight _fontMap )
              )
