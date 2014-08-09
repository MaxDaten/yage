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
                                 , Font(fontName)
                                 , FontLibrary
                                 , makeLibrary, freeLibrary, withNewLibrary
                                 , loadFont)

import Yage.Images
import Yage.Image.SDF

import Yage.Texture.TextureAtlas
import qualified Yage.Texture.Atlas.Builder as Builder

import Yage.Geometry.D2.Rectangle as Rectangle


---------------------------------------------------------------------------------------------------

type FontData = (FontGlyph, TextureRegion)
data FontMarkup = FontMarkup
    { _horizontalSpacing :: Double
    , _verticalSpacing   :: Double
    }

makeLenses ''FontMarkup


data FontTexture = FontTexture
    { _font              :: !Font
    , _charRegionMap     :: !(Map Char FontData)
    , _charPadding       :: !Int
    , _fontMap           :: !(Image Pixel8)
    , _fontDescriptor    :: !(FontDescriptor)
    , _fontMarkup        :: !(FontMarkup)
    } deriving ( Typeable )

makeLenses ''FontTexture

makeFontTexture :: Font -> FontMarkup -> TextureAtlas Char (Image Pixel8) -> FontTexture
makeFontTexture font markup textureAtlas =
    let glyphM   = charMap font
        regionM  = textureAtlas^.atlasRegions
    in FontTexture
        { _font           = font
        , _charRegionMap  = unionRegionsWithGlyphs regionM glyphM
        , _charPadding    = textureAtlas^.atlasPadding
        , _fontMap        = textureAtlas^.atlasImage
        -- ^ the image as a backend for the font
        , _fontDescriptor = fontDescr font
        , _fontMarkup     = markup
        }
    where
        unionRegionsWithGlyphs = intersectionWith (flip (,))


generateFontBitmapTexture :: Font -> FontMarkup -> FontLoadMode -> [Char] -> Builder.AtlasSettings Pixel8 -> Either [(Char, Builder.AtlasError Char)] FontTexture
generateFontBitmapTexture font markup mode chars settings =
    let imgs          = sortBy descArea $ map (unsafePerformIO . generateCharImg font mode) chars `piz` chars
        (err, atlas)  = Builder.newImageAtlas settings imgs
    in if null err
        then Right $ makeFontTexture font markup $ Builder.buildTextureAtlas atlas
        else error $ show err
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
    adjustRegion = over mapped (`div` scalefactor)
