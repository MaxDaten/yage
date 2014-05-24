{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Material
    ( module Yage.Material
    , module Material
    , Cube(..)
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Images              as Material
import Yage.Color               as Material
import Yage.Rendering.Resources as Material
import Yage.Resources


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
type AResourceMaterial f = Material (f TextureResource)
type ResourceMaterial = AResourceMaterial Identity

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


defaultMaterial :: Applicative f => MaterialPixel pixel => TextureCtr pixel -> AResourceMaterial f
defaultMaterial ctr =
    Material
    { _matColor   = opaque white
    , _matTexture = pure . TexturePure $ Texture "WHITEDUMMY" $ Texture2D (whiteDummy ctr)
    }


defaultMaterialSRGB :: Applicative f => AResourceMaterial f
defaultMaterialSRGB = defaultMaterial TexSRGB8


instance Applicative Material where
    pure tex = (runIdentity <$> defaultMaterialSRGB) & matTexture .~ tex
    matf <*> mat = mat & matTexture %~ (matf^.matTexture)


instance Applicative f => Monoid (AResourceMaterial f) where
    mempty = defaultMaterialSRGB
    (Material col _tex) `mappend` (Material col' tex') = Material (col <> col') (tex')


instance Applicative f => Default (AResourceMaterial f) where
    def = defaultMaterialSRGB


instance HasResources vert RenderMaterial RenderMaterial where
    requestResources = return


instance HasResources vert (AResourceMaterial Identity) RenderMaterial where
    requestResources mat =
        set matTexture <$> requestTextureResource (runIdentity $ mat^.matTexture)
                       <*> pure mat

instance HasResources vert (AResourceMaterial Cube) RenderMaterial where
    requestResources mat = do
        cubeTexs <- mapM requestTextureResource (mat^.matTexture)
        
        let cubeImgs = cubeTexs & mapped %~ ( getTextureImg . textureData )
            Just ( Texture baseName _ ) = firstOf traverse $ cubeTexs
        return $ mat & matTexture .~ ( Texture (baseName ++ "-Cube") $ TextureCube cubeImgs )
        
        where
        getTextureImg (Texture2D img) = img
        getTextureImg _ = error "requestResources: invalid TextureData"
