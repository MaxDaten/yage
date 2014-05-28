{-# LANGUAGE DataKinds, TypeOperators #-}
-- | Uniforms for Light
module Yage.Uniforms.Light where


import Data.Vinyl
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


lightPosition :: YLightPosition
lightPosition = Field

lightRadius :: YLightRadius
lightRadius = Field

lightColor :: YLightColor
lightColor = Field

lightAtten :: YLightAttenuation
lightAtten = Field

lightSpecExp :: YSpecularExp
lightSpecExp = Field

