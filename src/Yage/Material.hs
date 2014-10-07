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
import Yage.Resources
import Yage.Transformation           as Material

import Control.Comonad               as Comonad ( extract )
import Linear (V3(..), (^*), Quaternion)



type MaterialTexture     = TextureResource
type MaterialColor       = Colour Double
type MaterialColorAlpha  = AlphaColour Double
type MaterialPixel       = ColourPixel Double
-- | resulting texel color will be matColor * matTexture
-- so white matColor will result in 100% color from matTexture
data Material col tex = Material
    { _matColor          :: !col
    , _matTexture        :: !tex
    , _matTransformation :: !( Transformation Double )
    , _matConfig         :: !TextureConfig
    } deriving ( Functor, Foldable, Traversable )

makeLenses ''Material


-- material either to load or already loaded
type FMaterial f col tex = Material col (f tex)
type IMaterial col tex = FMaterial Identity col tex
type ResourceMaterial col = FMaterial Identity col TextureResource

-- ready to render material
type RenderMaterial col = IMaterial col Texture


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


mkMaterial :: Applicative f => col -> TextureResource -> FMaterial f col TextureResource
mkMaterial color texture = Material color (pure texture) idTransformation defaultTextureConfig


mkMaterialF :: col -> f TextureResource -> FMaterial f col TextureResource
mkMaterialF color textureF = Material color textureF idTransformation defaultTextureConfig


defaultMaterial :: (Applicative f, MaterialPixel pixel, Default col) => TextureCtr pixel -> FMaterial f col TextureResource
defaultMaterial ctr =
    let texture = TexturePure $ Texture "WHITEDUMMY" defaultTextureConfig $ Texture2D (whiteDummy ctr)
    in mkMaterial def texture


defaultMaterialSRGB :: Applicative f => FMaterial f MaterialColorAlpha TextureResource
defaultMaterialSRGB = defaultMaterial TexSRGB8


singleMaterial :: Lens' (ResourceMaterial col) TextureResource
singleMaterial = lens getter setter
    where
    getter = extract . _matTexture
    setter mat tex = mat & matTexture .~ ( pure tex )





--  Lens Shortcuts

stpFactor :: Lens' (Material col tex) (V3 Double)
stpFactor = matTransformation.transScale

stpOffset :: Lens' (Material col tex) (V3 Double)
stpOffset = matTransformation.transPosition

stpOrientation :: Lens' (Material col tex) (Quaternion Double)
stpOrientation = matTransformation.transOrientation

-- Instances

instance Applicative ( Material MaterialColorAlpha ) where
    pure tex = (fmap runIdentity defaultMaterialSRGB) & matTexture .~ tex
    matf <*> mat = mat & matTexture %~ (matf^.matTexture)


instance Applicative f => Monoid (FMaterial f MaterialColorAlpha TextureResource) where
    mempty = defaultMaterialSRGB
    (Material col _tex _trans _conf) `mappend` (Material col' tex' trans' conf') = Material (mappend col col') (tex') (trans') conf'


instance Applicative f => Default (FMaterial f MaterialColorAlpha TextureResource) where
    def = defaultMaterialSRGB


instance HasResources vert (RenderMaterial col) (RenderMaterial col) where
    requestResources = return


instance HasResources vert (FMaterial Identity col TextureResource) (RenderMaterial col) where
    requestResources mat =
        set matTexture . pure
            <$> ( requestTextureResource $ extract $ mat^.matTexture )
            <*> pure mat

instance HasResources vert (FMaterial Cube col TextureResource) (IMaterial col Texture) where
    requestResources mat = do
        cubeTexs <- mapM requestTextureResource (mat^.matTexture)

        let cubeImgs = cubeTexs & mapped %~ ( \tex -> getTextureImg $ tex^.textureData )
            Just ( Texture baseName _ _ ) = firstOf traverse $ cubeTexs
        return $ mat & matTexture .~ Identity (Texture ( baseName ++ "-Cube" ) ( mat^.matConfig ) ( TextureCube cubeImgs ))

        where
        getTextureImg (Texture2D img) = img
        getTextureImg _ = error "requestResources: invalid TextureData"

