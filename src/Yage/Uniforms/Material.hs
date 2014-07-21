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
type YMaterialTex t     = TextureUniform t
type YTextureMatrix m   = (m::Symbol) ::: M44 GLfloat

type YTextureSize t     = (t::Symbol) ::: V4 GLfloat

type YAlbedoTex         = YMaterialTex "AlbedoTexture"
type YAlbedoMaterial    = YMaterialUni "AlbedoColor" "AlbedoTextureMatrix"
type YAlbedoData        = YMaterialData YAlbedoMaterial YAlbedoTex

type YNormalTex         = YMaterialTex "NormalTexture"
type YNormalMaterial    = YMaterialUni "NormalColor" "NormalTextureMatrix"
type YNormalData        = YMaterialData YNormalMaterial YNormalTex

type YDepthTex          = YMaterialTex "DepthTexture"
type YDepthColor        = YMaterialColor "DepthColor"

type YSkyTex            = YMaterialTex "SkyTexture"
type YSkyMaterial       = YMaterialUni "SkyColor" "SkyTextureMatrix"
type YSkyData           = YMaterialData YSkyMaterial YSkyTex

type YAddTex            = YMaterialTex "AddTexture"
type YWhitePoint        = "WhitePoint" ::: GLfloat

materialColor :: KnownSymbol c => SField (YMaterialColor c)
materialColor = SField

materialTexture :: KnownSymbol t => SField (TextureUniform t)
materialTexture = SField

textureMatrix :: KnownSymbol m => SField (YTextureMatrix m)
textureMatrix = SField

textureSizeField ::KnownSymbol t => Texture -> Uniforms '[ YTextureSize t ]
textureSizeField tex = 
    let size = fromIntegral <$> (tex^.textureSpec.texSpecDimension) :: V2 GLfloat
    in SField =: (V4 (size^._x) (size^._y) (size^._x.to recip) (size^._y.to recip))


whitePoint :: SField YWhitePoint
whitePoint = SField

{--
Utility
--}

materialUniforms :: (KnownSymbol c, KnownSymbol m, KnownSymbol t) => RenderMaterial -> YMaterialData (YMaterialUni c m) (YMaterialTex t)
materialUniforms mat =
    let col = materialColor   =: ( realToFrac <$> mat^.matColor.to linearV4) <+>
              textureMatrix   =: ( (fmap.fmap) realToFrac $ mat^.matTransformation.transformationMatrix )
        tex = materialTexture =: (mat^.matTexture)
    in ShaderData col tex
