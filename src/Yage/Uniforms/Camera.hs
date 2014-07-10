{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms for Filming Effects and Camara
module Yage.Uniforms.Camera where


import Data.Vinyl
import Graphics.Rendering.OpenGL            (GLfloat)


type YExposure      = "Exposure"        ::: GLfloat
type YExposureBias  = "ExposureBias"    ::: GLfloat
type YGamma         = "Gamma"           ::: GLfloat
type YInverseGamma  = "InverseGamma"    ::: GLfloat




exposure :: YExposure
exposure = Field

exposureBias :: YExposureBias
exposureBias = Field

gamma :: YGamma
gamma = Field

inverseGamma :: YInverseGamma
inverseGamma = Field

