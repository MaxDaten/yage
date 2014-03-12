{-# LANGUAGE TypeOperators #-}
module Yage.Pipeline.Deferred.Spec
    ( module Yage.Pipeline.Deferred.Spec
    , module Uniforms
    ) where

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms

type PerspectiveUniforms = [YViewMatrix, YVPMatrix]

type GeoGlobalUniforms = PerspectiveUniforms ++ [YZFarPlane, YAlbedoTex, YNormalTex]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix] ++ YMaterial
type GeoVertex         = P3TX2NT3

type SkyUniforms      = [YModelMatrix, YIntensity, YSkyTexture]

type LitGlobalUniforms = PerspectiveUniforms ++ [YViewportDim, YZNearFarPlane, YZProjRatio, YAlbedoTex, YNormalTex, YDepthTex]
type LitLocalUniforms  = '[YModelMatrix] ++ YLightAttributes
type LitVertex         = P3

type ScrGlobalUniforms = [YProjectionMatrix, YScreenTex]
type ScrLocalUniforms  = '[YModelMatrix]
type ScrVertex         = P3TX2



