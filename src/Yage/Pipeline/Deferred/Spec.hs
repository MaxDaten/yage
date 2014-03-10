{-# LANGUAGE TypeOperators #-}
module Yage.Pipeline.Deferred.Spec
    ( module Yage.Pipeline.Deferred.Spec
    , module Uniforms
    ) where

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms


type GeoGlobalUniforms = [YViewMatrix, YVPMatrix, YZFarPlane, YAlbedoTex, YNormalTex]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix]
type GeoVertex         = P3TX2NT3


type LitGlobalUniforms = [YViewMatrix, YVPMatrix, YViewportDim, YZNearFarPlane, YAlbedoTex, YNormalTex, YDepthTex]
type LitLocalUniforms  = '[YModelMatrix] ++ YLightAttributes
type LitVertex         = P3

type ScrGlobalUniforms = [YProjectionMatrix, YScreenTex]
type ScrLocalUniforms  = '[YModelMatrix]
type ScrVertex         = P3TX2



