{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms fo Materials & Textures
module Yage.Uniforms.Material where

import Yage.Prelude
import Yage.Lens

import Data.Vinyl
import Graphics.Rendering.OpenGL            (GLfloat, GLint)
import Linear

import Yage.Rendering.Uniforms

import Yage.Material
import Yage.Color

type YMaterial c t      = [ YMaterialColor c, YMaterialTexture t ]

type YMaterialColor c   = c                     ::: V4 GLfloat
type YMaterialTexture t = t                     ::: GLint


type YAlbedoTex         = YMaterialTexture "AlbedoTexture"
type YNormalTex         = YMaterialTexture "NormalTexture"
type YTangentTex        = YMaterialTexture "TangentTexture"
type YDepthTex          = YMaterialTexture "DepthTexture"
type YSkyTexture        = YMaterialTexture "SkyTexture"
type YScreenTex         = YMaterialTexture "ScreenTexture"

type YIntensity         = "intensity"            ::: GLfloat

{--
Fields
--}

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

intensity :: YIntensity
intensity = Field


materialColor :: YMaterialColor c
materialColor = Field

materialTexture :: YMaterialTexture t
materialTexture = Field


{--
Utility
--}


materialUniforms :: GLint -> Material -> Uniforms (YMaterial c t)
materialUniforms ch mat =
    materialColor   =: (realToFrac <$> mat^.matColor.to linearV4) <+>
    materialTexture =: ch -- TODO  {-- mat^.matTexture --}

