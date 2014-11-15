{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
-- | Uniforms fo Materials & Textures
module Yage.Uniforms.Material where

--{--
import Yage.Prelude
import Yage.Lens

import Data.Vinyl.Universe
import Graphics.Rendering.OpenGL            (GLfloat)
import Linear

import Yage.Rendering.Shader

import Yage.Material


type YMaterialData m t = ShaderData m '[t]

type YMaterialUni c m   = [ YMaterialColor c, YTextureMatrix m ]

type YMaterialColor c   = (c::Symbol) ::: V4 GLfloat
type YMaterialIntensity c = (c::Symbol) ::: V1 GLfloat
type YMaterialTex t     = TextureSampler t
type TextureLod t       = (t::Symbol) ::: V1 GLfloat
type YTextureMatrix m   = (m::Symbol) ::: M44 GLfloat

type YTextureSize t     = (t::Symbol) ::: V4 GLfloat

type YAlbedoTex         = YMaterialTex "AlbedoTexture"
type YAlbedoMaterial    = YMaterialUni "AlbedoColor" "AlbedoTextureMatrix"
type YAlbedoData        = YMaterialData YAlbedoMaterial YAlbedoTex

type YNormalTex         = YMaterialTex "NormalTexture"
type YNormalMaterial    = YMaterialUni "NormalColor" "NormalTextureMatrix"
type YNormalData        = YMaterialData YNormalMaterial YNormalTex

type YRoughnessTex      = YMaterialTex "RoughnessTexture"
type YRoughnessMaterial = [ YMaterialIntensity "RoughnessIntensity", YTextureMatrix "RoughnessTextureMatrix" ]
type YRoughnessData     = YMaterialData YRoughnessMaterial YRoughnessTex

type YDepthTex          = YMaterialTex "DepthTexture"
type YDepthColor        = YMaterialColor "DepthColor"

type YEnvironmentCubeMap    = YMaterialTex "EnvironmentCubeMap"

type YSkyTex            = YMaterialTex "SkyTexture"
type YSkyMaterial       = YMaterialUni "SkyColor" "SkyTextureMatrix"
type YSkyData           = YMaterialData YSkyMaterial YSkyTex

type YAddTex            = YMaterialTex "AddTexture"
type YWhitePoint        = "WhitePoint" ::: GLfloat

type YStepDirection     = "Direction" ::: V2 GLfloat

materialColor :: KnownSymbol c => SField (YMaterialColor c)
materialColor = SField

materialIntensity :: KnownSymbol c => SField (YMaterialIntensity c)
materialIntensity = SField

textureSampler :: KnownSymbol t => SField (TextureSampler t)
textureSampler = SField

textureArray :: KnownSymbol t => SField (TextureArray t)
textureArray = SField

textureLod :: KnownSymbol t => SField (TextureLod t)
textureLod = SField

textureMatrix :: KnownSymbol m => SField (YTextureMatrix m)
textureMatrix = SField

textureSizeField ::KnownSymbol t => Texture -> Uniforms '[ YTextureSize t ]
textureSizeField tex =
    let size = fromIntegral <$> (tex^.textureSpec.texSpecDimension) :: V2 GLfloat
    in SField =: (V4 (size^._x) (size^._y) (size^._x.to recip) (size^._y.to recip))


whitePoint :: SField YWhitePoint
whitePoint = SField

stepDirection :: SField YStepDirection
stepDirection = SField

{--
Utility
--}

materialUniformsColor :: ( KnownSymbol c, KnownSymbol m, KnownSymbol t ) => Material MaterialColorAlpha -> YMaterialData (YMaterialUni c m) (YMaterialTex t)
materialUniformsColor mat =
    let col = materialColor   =: ( realToFrac <$> mat^.matColor.to linearV4) <+>
              textureMatrix   =: ( (fmap.fmap) realToFrac $ mat^.matTransformation.transformationMatrix )
        tex = textureSampler =: (mat^.matTexture)
    in ShaderData col tex


materialUniformsIntensity :: ( KnownSymbol c, KnownSymbol m, KnownSymbol t ) => Material Double -> YMaterialData ([ YMaterialIntensity c, YTextureMatrix m ]) (YMaterialTex t)
materialUniformsIntensity mat =
    let intensity = materialIntensity   =: ( realToFrac <$> V1 ( mat^.matColor ) ) <+>
                    textureMatrix       =: ( (fmap.fmap) realToFrac $ mat^.matTransformation.transformationMatrix )
        tex = textureSampler =: (mat^.matTexture)
    in ShaderData intensity tex
