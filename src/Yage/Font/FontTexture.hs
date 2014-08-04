{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Yage.Font.FontTexture
    ( module Yage.Font.FontTexture
    , module Rectangle
    , module FTExport
    ) where

import Yage.Prelude hiding (Text)
import Yage.Lens
import Data.ByteString.Lens

import System.IO.Unsafe (unsafePerformIO)

import Yage.Data.List (piz)
import Data.Map hiding (map, null)

import Graphics.Font as FT
import Graphics.Font as FTExport ( FontLoadMode(..)
                                 , FontDescriptor(..)
                                 , Font(fontName)
                                 , FontLibrary
                                 , makeLibrary, freeLibrary, withNewLibrary
                                 , loadFont)

import Yage.Images
import Yage.Material
import Yage.Texture.Atlas
import Yage.Texture.Atlas.Builder
import Yage.Geometry.D2.Rectangle as Rectangle


---------------------------------------------------------------------------------------------------

type FontData = (FontGlyph, TextureRegion)
data FontMarkup = FontMarkup
    { _horizontalSpacing :: Double
    , _verticalSpacing   :: Double
    }

makeLenses ''FontMarkup


data FontTexture = FontTexture
    { _font              :: Font
    , _charRegionMap     :: Map Char FontData
    , _charPadding       :: Int
    , _fontMap           :: Texture
    , _fontDescriptor    :: FontDescriptor
    , _fontMarkup        :: FontMarkup
    } deriving ( Typeable )

makeLenses ''FontTexture

makeFontTexture :: Font -> FontMarkup -> TextureAtlas Char Pixel8 -> FontTexture
makeFontTexture font markup filedAtlas =
    let glyphM = charMap font
        regionM  = regionMap filedAtlas
        img      = mkTextureImg TexY8 $ atlasToImage filedAtlas
    in FontTexture
        { _font           = font
        , _charRegionMap  = unionRegionsWithGlyphs regionM glyphM
        , _charPadding    = filedAtlas^.atlasSettings.atlPaddingPx
        , _fontMap        = Texture (font^.to fontName.packedChars) def $ Texture2D img
        , _fontDescriptor = fontDescr font
        , _fontMarkup     = markup
        }
    where
        unionRegionsWithGlyphs = intersectionWith (flip (,))

generateFontBitmapTexture :: Font -> FontMarkup -> FontLoadMode -> [Char] -> TextureAtlas Char Pixel8 -> Either [(Char, AtlasError Char)] FontTexture
generateFontBitmapTexture font markup mode chars emptyAtlas =
    let imgs          = sortBy descArea $ map (unsafePerformIO . generateCharImg font mode) chars `piz` chars
        (err, atlas)  = insertImages imgs emptyAtlas
    in if null err
        then Right $ makeFontTexture font markup atlas
        else error $ show err
    where
        descArea (_, img1) (_, img2) = descending imageByAreaCompare img1 img2


calculateFontNDFTexture :: FontTexture -> Int -> FontTexture
calculateFontNDFTexture fontTex scalefactor = undefined
