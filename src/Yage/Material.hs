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
import Yage.Images                   as Material
import Yage.Color                    as Material
import Yage.Rendering.Resources      as Material
import Yage.Resources
import Yage.Transformation           as Material

import Linear (V3, Quaternion)



type MaterialTexture = TextureResource
type MaterialColor   = Colour Double
type MaterialColorA  = AlphaColour Double
type MaterialPixel   = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material tex = Material
    { _matColor          :: !MaterialColorA
    , _matTexture        :: !tex
    , _matTransformation :: !( Transformation Double )
    , _matConfig         :: !TextureConfig
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


mkMaterial :: Applicative f => MaterialColorA -> TextureResource -> AResourceMaterial f
mkMaterial color texture = Material color (pure texture) idTransformation defaultTextureConfig

mkMaterialF :: MaterialColorA -> f TextureResource -> AResourceMaterial f
mkMaterialF color textureF = Material color textureF idTransformation defaultTextureConfig

defaultMaterial :: Applicative f => MaterialPixel pixel => TextureCtr pixel -> AResourceMaterial f
defaultMaterial ctr =
    let color   = opaque white
        texture = TexturePure $ Texture "WHITEDUMMY" defaultTextureConfig $ Texture2D (whiteDummy ctr)
    in mkMaterial color texture


defaultMaterialSRGB :: Applicative f => AResourceMaterial f
defaultMaterialSRGB = defaultMaterial TexSRGB8


singleMaterial :: Lens' ResourceMaterial TextureResource
singleMaterial = lens getter setter
    where
    getter = runIdentity . _matTexture
    setter mat tex = mat & matTexture .~ ( pure tex )


--  Lens Shortcuts

stpFactor :: Lens' (Material tex) (V3 Double)
stpFactor = matTransformation.transScale

stpOffset :: Lens' (Material tex) (V3 Double)
stpOffset = matTransformation.transPosition

stpOrientation :: Lens' (Material tex) (Quaternion Double)
stpOrientation = matTransformation.transOrientation

-- Instances

instance Applicative Material where
    pure tex = (runIdentity <$> defaultMaterialSRGB) & matTexture .~ tex
    matf <*> mat = mat & matTexture %~ (matf^.matTexture)


instance Applicative f => Monoid (AResourceMaterial f) where
    mempty = defaultMaterialSRGB
    (Material col _tex _trans _conf) `mappend` (Material col' tex' trans' conf') = Material (col <> col') (tex') (trans') conf'


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

        let cubeImgs = cubeTexs & mapped %~ ( \tex -> getTextureImg $ tex^.textureData )
            Just ( Texture baseName _ _ ) = firstOf traverse $ cubeTexs
        return $ mat & matTexture .~ Texture ( baseName ++ "-Cube" ) ( mat^.matConfig ) ( TextureCube cubeImgs )

        where
        getTextureImg (Texture2D img) = img
        getTextureImg _ = error "requestResources: invalid TextureData"
