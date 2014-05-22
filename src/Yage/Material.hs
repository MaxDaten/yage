{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Material where

import Yage.Prelude
import Yage.Lens
import Yage.Images
import Yage.Color
import Yage.Resources
import Yage.Rendering.Resources

type MaterialTexture = TextureResource 
type MaterialColor   = Colour Double
type MaterialColorA  = AlphaColour Double
type MaterialPixel   = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material tex = Material
    { _matColor    :: !MaterialColorA
    , _matTexture  :: !tex
    } deriving ( Functor )

makeLenses ''Material


-- material either to load or already loaded
type ResourceMaterial = Material TextureResource

-- ready to render material
type RenderMaterial = Material Texture



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


defaultMaterial :: MaterialPixel pixel => TextureCtr pixel -> RenderMaterial
defaultMaterial ctr =
    Material
    { _matColor   = opaque white
    , _matTexture = Texture "WHITEDUMMY" $ Texture2D (whiteDummy ctr)
    }


instance HasResources vert ResourceMaterial RenderMaterial where
    requestResources mat =
        set matTexture <$> requestTextureResource (mat^.matTexture)
                       <*> pure mat

