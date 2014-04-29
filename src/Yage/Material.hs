{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
module Yage.Material where

import Yage.Prelude
import Yage.Lens
import Yage.Images
import Yage.Color

type MaterialTexture = TextureImage 
type MaterialColor   = Colour Double
type MaterialColorA  = AlphaColour Double
type MaterialPixel   = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material = Material
    { _matColor    :: !MaterialColorA
    , _matTexture  :: !MaterialTexture
    }

makeLenses ''Material


zeroNormal :: (Ord a, Floating a) => Colour a
zeroNormal = rgb 0.5 0.5 0.5


pxTexture :: MaterialPixel pixel => TextureCtr pixel -> MaterialColor -> TextureImage
pxTexture ctr = mkTextureImg ctr . constColorPx 


whiteDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
whiteDummy = (`pxTexture` white)


blackDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
blackDummy = (`pxTexture` black)


redDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
redDummy = (`pxTexture` red)


zeroNormalDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
zeroNormalDummy = (`pxTexture` zeroNormal)


defaultMaterial :: MaterialPixel pixel => TextureCtr pixel -> Material
defaultMaterial ctr =
    Material
    { _matColor   = opaque white
    , _matTexture = whiteDummy ctr
    }


