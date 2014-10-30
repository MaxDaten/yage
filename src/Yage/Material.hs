{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Material
    ( module Yage.Material
    , module Material
    , module Comonad
    , Cube(..)
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Images                   as Material
import Yage.Color                    as Material
import Yage.Rendering.Resources      as Material
import Yage.Transformation           as Material

import Control.Comonad               as Comonad ( extract )
import Linear (V3(..), (^*), Quaternion)


type MaterialColor       = Colour Double
type MaterialColorAlpha  = AlphaColour Double
type MaterialPixel       = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material col = Material
    { _matColor          :: !col
    , _matTexture        :: !Texture
    , _matTransformation :: !( Transformation Double )
    }

makeLenses ''Material


pxTexture :: MaterialPixel pixel => TextureCtr pixel -> MaterialColor -> TextureImage
pxTexture ctr = mkTextureImg ctr . constColorPx


whiteDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
whiteDummy = (`pxTexture` white)


blackDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
blackDummy = (`pxTexture` black)


redDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
redDummy = (`pxTexture` red)


-- | 1px normal `TextureImage`
--
-- asumes an already normalized argument
constantNormal :: MaterialPixel pixel => V3 Double -> TextureCtr pixel -> TextureImage
constantNormal normal ctr =
    let (V3 nx ny nz) = normal ^* 0.5 + 0.5
    in ctr `pxTexture` (rgb nx ny nz)


zeroNormalDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
zeroNormalDummy = constantNormal (V3 0 0 0)


zNormalDummy :: MaterialPixel pixel => TextureCtr pixel -> TextureImage
zNormalDummy = constantNormal (V3 0 0 1)


mkMaterial :: col -> Texture -> Material col
mkMaterial color texture = Material color texture idTransformation


defaultMaterial :: (MaterialPixel pixel, Default col) => TextureCtr pixel -> Material col
defaultMaterial ctr = mkMaterial def $ mkTexture2D "WHITEDUMMY" (whiteDummy ctr)


defaultMaterialSRGB :: Material MaterialColorAlpha
defaultMaterialSRGB = defaultMaterial TexSRGB8


instance Default (Material MaterialColorAlpha) where
    def = defaultMaterialSRGB

--  Lens Shortcuts & Aliases

stpFactor :: Lens' (Material col) (V3 Double)
stpFactor = matTransformation.transScale

stpOffset :: Lens' (Material col) (V3 Double)
stpOffset = matTransformation.transPosition

stpOrientation :: Lens' (Material col) (Quaternion Double)
stpOrientation = matTransformation.transOrientation

