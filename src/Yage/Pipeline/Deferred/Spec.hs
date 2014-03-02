module Yage.Pipeline.Deferred.Spec
    ( module Yage.Pipeline.Deferred.Spec
    , module Uniforms
    ) where

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms


type GeoGlobalUniforms = [YProjectionMatrix, YViewMatrix, YVPMatrix]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix]
type GeoVertex         = P3T2


type LitGlobalUniforms = '[]
type LitLocalUniforms  = '[]
type LitVertex         = P3N3T2

type ScrGlobalUniforms = [YProjectionMatrix, YScreenTex]
type ScrLocalUniforms  = '[YModelMatrix]
type ScrVertex         = P3T2

