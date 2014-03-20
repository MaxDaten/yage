{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Yage.Uniforms
    ( module Yage.Uniforms
    , module Yage.Rendering.Uniforms
    ) where


import Data.Vinyl
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Yage.Rendering.Uniforms
import Linear


type YProjectionMatrix = "ProjMatrix"           ::: M44 GLfloat
type YViewMatrix       = "ViewMatrix"           ::: M44 GLfloat
type YModelMatrix      = "ModelMatrix"          ::: M44 GLfloat
type YNormalMatrix     = "NormalMatrix"         ::: M33 GLfloat

type YVPMatrix         = "VPMatrix"             ::: M44 GLfloat
type YMVPMatrix        = "MVPMatrix"            ::: M44 GLfloat

type YAlbedoTex        = "AlbedoTexture"        ::: GLint
type YNormalTex        = "NormalTexture"        ::: GLint
type YTangentTex       = "TangentTexture"       ::: GLint
type YDepthTex         = "DepthTexture"         ::: GLint
type YSkyTexture       = "SkyTexture"           ::: GLint
type YIntensity        = "intensity"            ::: GLfloat

type YScreenTex        = "ScreenTexture"        ::: GLint

type YViewportDim      = "ViewportDim"          ::: V2 GLint
type YEyePosition      = "EyePosition"          ::: V3 GLfloat
type YZNearPlane       = "ZNear"                ::: GLfloat
type YZFarPlane        = "ZFar"                 ::: GLfloat
type YZNearFarPlane    = "ZNearFar"             ::: V2 GLfloat
type YZProjRatio       = "ZProjRatio"           ::: V2 GLfloat
type YFieldOfView      = "Fovy"                 ::: GLfloat

type YTextured         = "Textured"             ::: GLint
type YMaterialColor    = "MaterialColor"        ::: V3 GLfloat
type YMaterialSpecular = "MaterialSpecular"     ::: GLfloat

type YLightPosition      = "lightPosition"      ::: V3 GLfloat
type YLightRadius        = "lightRadius"        ::: V3 GLfloat
type YLightColor         = "lightColor"         ::: V4 GLfloat
type YLightAttenuation   = "lightAttenuation"   ::: V3 GLfloat
type YSpecularExp        = "lightSpecularExp"   ::: GLfloat

type YLightAttributes    = [ YLightPosition, YLightRadius
                           , YLightColor
                           , YLightAttenuation
                           , YSpecularExp ]

type YMaterial = [ YTextured, YMaterialColor, YMaterialSpecular ]


projectionMatrix :: YProjectionMatrix
projectionMatrix = Field

viewMatrix :: YViewMatrix
viewMatrix = Field

modelMatrix :: YModelMatrix
modelMatrix = Field

vpMatrix :: YVPMatrix
vpMatrix = Field

mvpMatrix :: YMVPMatrix
mvpMatrix = Field

normalMatrix :: YNormalMatrix
normalMatrix = Field

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

viewportDim :: YViewportDim
viewportDim = Field

eyePosition :: YEyePosition
eyePosition = Field

zNearPlane :: YZNearPlane
zNearPlane = Field

zFarPlane :: YZFarPlane
zFarPlane = Field

zNearFarPlane :: YZNearFarPlane
zNearFarPlane = Field

zProjRatio :: YZProjRatio
zProjRatio = Field

fieldOfView :: YFieldOfView
fieldOfView = Field

{--
Material
--}

textured :: YTextured
textured = Field

materialColor :: YMaterialColor
materialColor = Field

materialSpecular :: YMaterialSpecular
materialSpecular = Field

{--
Light
--}

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