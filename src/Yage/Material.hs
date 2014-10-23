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
    , _matConfig         :: !TextureConfig
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
mkMaterial color texture = Material color texture idTransformation defaultTextureConfig



defaultMaterial :: (MaterialPixel pixel, Default col) => TextureCtr pixel -> Material col
defaultMaterial ctr =
    let texture = Texture "WHITEDUMMY" defaultTextureConfig $ Texture2D (whiteDummy ctr)
    in mkMaterial def texture


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


{--

instance HasResources vert (RenderMaterial col) (RenderMaterial col) where
    requestResources = return


instance HasResources vert (FMaterial Identity col Texture) (RenderMaterial col) where
    requestResources mat =
        set matTexture . pure
            <$> ( requestTexture $ extract $ mat^.matTexture )
            <*> pure mat

instance HasResources vert (FMaterial Cube col Texture) (IMaterial col Texture) where
    requestResources mat = do
        cubeTexs <- mapM requestTexture (mat^.matTexture)

        let cubeImgs = cubeTexs & mapped %~ ( \tex -> getTextureImg $ tex^.textureData )
            Just ( Texture baseName _ _ ) = firstOf traverse $ cubeTexs
        return $ mat & matTexture .~ Identity (Texture ( baseName ++ "-Cube" ) ( mat^.matConfig ) ( TextureCube cubeImgs ))

        where
        getTextureImg (Texture2D img) = img
        getTextureImg _ = error "requestResources: invalid TextureData"
--}

