{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms fo Materials & Textures
module Yage.Uniforms.Material where

--{--
import Yage.Prelude
import Yage.Lens

import Data.Vinyl
import Graphics.Rendering.OpenGL            (GLfloat)
import Linear

import Yage.Rendering.Shader

import Yage.Material



type YMaterialData m t = ShaderData m '[t]

type YMaterialUni c m   = [ YMaterialColor c, YTextureMatrix m ]

type YMaterialColor c   = c ::: V4 GLfloat
type YMaterialTex t     = TextureUniform t
type YTextureMatrix m   = m ::: M44 GLfloat

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

type YScreenTex         = YMaterialTex "ScreenTexture"


{--
Fields
--}
{--
screenTex :: YScreenTex
screenTex = Field

albedoTex :: YAlbedoTex
albedoTex = Field

normalTex :: YNormalTex
normalTex = Field

tangentTex :: YTangentTex
tangentTex = Field

depthTex :: YDepthTex
depthTex = Field

skyTex :: YSkyTexture
skyTex = Field

--}

materialColor :: YMaterialColor c
materialColor = Field

materialTexture :: TextureUniform t
materialTexture = Field

textureMatrix :: YTextureMatrix m
textureMatrix = Field


{--
Utility
--}

materialUniforms :: RenderMaterial -> YMaterialData (YMaterialUni c m) (YMaterialTex t)
materialUniforms mat =
    let col = materialColor   =: ( realToFrac <$> mat^.matColor.to linearV4) <+>
              textureMatrix   =: ( (fmap.fmap) realToFrac $ calcModelMatrix $ mat^.matTransformation )
        tex = materialTexture =: (mat^.matTexture)
    in ShaderData col tex
