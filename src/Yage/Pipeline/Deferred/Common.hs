{-# LANGUAGE TypeOperators #-}
module Yage.Pipeline.Deferred.Common
    ( module Yage.Pipeline.Deferred.Common
    , module Uniforms
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms
import Yage.Camera

import Yage.Rendering.Viewport
import Yage.Rendering.Transformation


type PerspectiveUniforms = [YViewMatrix, YVPMatrix]


perspectiveUniforms :: (Integral a) => Viewport a -> Camera -> Uniforms PerspectiveUniforms
perspectiveUniforms vp cam =
    let projM = cameraProjectionMatrix cam (fromIntegral <$> vp)
        viewM = (fmap . fmap) realToFrac (cam^.cameraHandle.to camMatrix)
        vpM   = projM !*! viewM
    in viewMatrix       =: viewM <+>
       vpMatrix         =: vpM


calcModelMatrix :: (Num a) => Transformation a -> M44 a
calcModelMatrix trans =
    let scaleM       = kronecker . point $ trans^.transScale
        transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
    in transM !*! scaleM
