{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Yage.Material
    ( module Yage.Material
    , module Material
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Image                    as Material
import Yage.Color                    as Material
import Yage.Transformation           as Material

import Linear (V3(..), (^*), Quaternion)


type MaterialColor       = Colour Double
type MaterialColorAlpha  = AlphaColour Double
type MaterialPixel       = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material col tex = Material
    { _matColor          :: col
    , _matTexture        :: tex
    , _matTransformation :: Transformation Double
    }

makeLenses ''Material


whiteDummy :: MaterialPixel a => Image a
whiteDummy = constColorPx (white :: Colour Double)


blackDummy :: MaterialPixel a => Image a
blackDummy = constColorPx (black :: Colour Double)


redDummy :: MaterialPixel a => Image a
redDummy = constColorPx (red :: Colour Double)


-- | 1px normal `TextureImage`
--
-- asumes an already normalized argument
constantNormal :: MaterialPixel a => V3 Double -> Image a
constantNormal normal =
    let (V3 nx ny nz) = normal ^* 0.5 + 0.5
    in constColorPx (rgb nx ny nz)


zeroNormalDummy :: MaterialPixel a => Image a
zeroNormalDummy = constantNormal (V3 0 0 0)


zNormalDummy :: MaterialPixel a => Image a
zNormalDummy = constantNormal (V3 0 0 1)


mkMaterial :: col -> Image a -> Material col (Image a)
mkMaterial color texture = Material color texture idTransformation


defaultMaterial :: (MaterialPixel a, Default col) => Material col (Image a)
defaultMaterial = mkMaterial def whiteDummy


defaultMaterialSRGB :: Material MaterialColorAlpha (Image PixelRGB8)
defaultMaterialSRGB = defaultMaterial


instance Default (Material MaterialColorAlpha (Image PixelRGB8)) where
    def = defaultMaterialSRGB

--  Lens Shortcuts & Aliases

stpFactor :: Lens' (Material col i) (V3 Double)
stpFactor = matTransformation.transScale

stpOffset :: Lens' (Material col i) (V3 Double)
stpOffset = matTransformation.transPosition

stpOrientation :: Lens' (Material col i) (Quaternion Double)
stpOrientation = matTransformation.transOrientation

