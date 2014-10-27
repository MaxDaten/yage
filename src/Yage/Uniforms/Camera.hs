{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms for Filming Effects and Camara
module Yage.Uniforms.Camera where


import Data.Vinyl.Universe
import Graphics.Rendering.OpenGL            (GLfloat)


type YExposure          = "Exposure"        ::: GLfloat
type YExposureBias      = "ExposureBias"    ::: GLfloat
type YBloomThreshold    = "BloomThreshold"  ::: GLfloat
type YGamma             = "Gamma"           ::: GLfloat
type YInverseGamma      = "InverseGamma"    ::: GLfloat




exposure :: SField YExposure
exposure = SField

exposureBias :: SField YExposureBias
exposureBias = SField

bloomthreshold :: SField YBloomThreshold
bloomthreshold = SField

gamma :: SField YGamma
gamma = SField

inverseGamma :: SField YInverseGamma
inverseGamma = SField

