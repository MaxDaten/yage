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


type YMaterial c t      = ShaderData '[ YMaterialColor c ] '[ TextureUniform t ]

type YMaterialColor c   = c ::: V4 GLfloat
type YMaterialTex t     = TextureUniform t

type YAlbedoTex         = TextureUniform "AlbedoTexture"
type YAlbedoColor       = YMaterialColor "AlbedoColor"

type YNormalTex         = TextureUniform "NormalTexture"
type YNormalColor       = YMaterialColor "NormalColor"

type YTangentTex        = TextureUniform "TangentTexture"
type YTangentColor      = YMaterialColor "TangentColor"

type YDepthTex          = TextureUniform "DepthTexture"
type YDepthColor        = YMaterialColor "DepthColor"

type YSkyTex            = TextureUniform "SkyTexture"
type YSkyColor          = YMaterialColor "SkyColor"

type YScreenTex         = TextureUniform "ScreenTexture"


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


{--
Utility
--}

materialUniforms :: RenderMaterial -> YMaterial c t
materialUniforms mat =
    let col = materialColor   =: (realToFrac <$> mat^.matColor.to linearV4)
        tex = materialTexture =: (mat^.matTexture)
    in ShaderData col tex
