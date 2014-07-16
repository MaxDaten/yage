{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms for Light
module Yage.Uniforms.Light where


import Data.Vinyl.Universe
import Graphics.Rendering.OpenGL            (GLfloat)
import Linear


type YLightPosition      = "lightPosition"        ::: V3 GLfloat
type YLightRadius        = "lightRadius"          ::: V3 GLfloat
type YLightColor         = "lightColor"           ::: V4 GLfloat
type YLightAttenuation   = "lightAttenuation"     ::: V3 GLfloat
type YSpecularExp        = "lightSpecularExp"     ::: GLfloat

type YLightAttributes    = [ YLightPosition, YLightRadius
                           , YLightColor
                           , YLightAttenuation
                           , YSpecularExp 
                           ]


lightPosition :: SField YLightPosition
lightPosition = SField

lightRadius :: SField YLightRadius
lightRadius = SField

lightColor :: SField YLightColor
lightColor = SField

lightAtten :: SField YLightAttenuation
lightAtten = SField

lightSpecExp :: SField YSpecularExp
lightSpecExp = SField

