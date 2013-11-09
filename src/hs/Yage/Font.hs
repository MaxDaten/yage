{-# LANGUAGE TemplateHaskell #-}
module Yage.Font
    ( module Yage.Font
    , module FTExport
    ) where

import Yage.Prelude hiding (Text)
import Yage.Images
import Yage.Math
import GHC.Float
import Foreign.C.Types


import Data.Map hiding (map, null)
import Data.List (map, sortBy, null)
import Data.Digest.Pure.SHA
import Control.Lens hiding (indices)

import Graphics.Font as FT
import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)

import Yage.Texture.Atlas
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives
import Linear

import Codec.Picture.Types

---------------------------------------------------------------------------------------------------


data FontTexture a = FontTexture
    { _fontName          :: String
    , _charRegionMap     :: RegionMap Char
    , _textureData       :: DynamicImage
    }

makeLenses ''FontTexture

makeFontTexture :: String -> TextureAtlas Char Pixel8 -> FontTexture a
makeFontTexture fontname filedAtlas = FontTexture
    { _fontName      = fontname
    , _charRegionMap = regionMap filedAtlas
    , _textureData   = ImageY8 $ atlasToImage filedAtlas
    }

generateFontTexture :: Font -> FontLoadMode -> [Char] -> TextureAtlas Char Pixel8 -> Either [(Char, AtlasError Char)] (FontTexture Pixel8)
generateFontTexture font mode chars emptyAtlas =
    let imgs          = sortBy descArea $ (map (generateCharImg font mode) chars) `piz` chars
        (err, atlas)  = insertImages imgs emptyAtlas
        name          = fontname font
    in if null err then Right $ makeFontTexture name atlas else Left err 
    where
        descArea (_, img1) (_, img2) = descending imageByAreaCompare img1 img2

