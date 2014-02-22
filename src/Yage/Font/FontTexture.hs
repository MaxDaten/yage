{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Font.FontTexture
    ( module Yage.Font.FontTexture
    , module FTExport
    ) where

import Yage.Prelude hiding (Text)
import Yage.Images


import Yage.Data.List (map, sortBy, null, piz)
import Data.Map hiding (map, null)

import Graphics.Font as FT
import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font(fontname), loadFont)

import Yage.Texture.Atlas

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
    , _textureData       :: DynamicImage
    , _fontDescriptor    :: FontDescriptor
    , _fontMarkup        :: FontMarkup
    }

makeLenses ''FontTexture

makeFontTexture :: Font -> FontMarkup -> TextureAtlas Char Pixel8 -> FontTexture
makeFontTexture font markup filedAtlas =
    let glyphM = charMap font
        regionM  = regionMap filedAtlas
    in FontTexture
        { _font           = font
        , _charRegionMap  = unionRegionsWithGlyphs regionM glyphM
        , _textureData    = ImageY8 $ atlasToImage filedAtlas
        , _fontDescriptor = fontDescr font 
        , _fontMarkup     = markup
        }
    where
        unionRegionsWithGlyphs = intersectionWith (flip (,))

generateFontTexture :: Font -> FontMarkup -> FontLoadMode -> [Char] -> TextureAtlas Char Pixel8 -> Either [(Char, AtlasError Char)] FontTexture
generateFontTexture font markup mode chars emptyAtlas =
    let imgs          = sortBy descArea $ map (generateCharImg font mode) chars `piz` chars
        (err, atlas)  = insertImages imgs emptyAtlas
    in if null err then Right $ makeFontTexture font markup atlas else error $ show err 
    where
        descArea (_, img1) (_, img2) = descending imageByAreaCompare img1 img2

