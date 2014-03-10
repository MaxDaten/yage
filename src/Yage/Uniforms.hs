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

type YScreenTex        = "ScreenTexture"        ::: GLint

type YViewportDim      = "ViewportDim"           ::: V2 GLint
type YEyePosition      = "EyePosition"           ::: V3 GLfloat
type YZNearPlane       = "ZNear"                 ::: GLfloat
type YZFarPlane        = "ZFar"                  ::: GLfloat
type YZNearFarPlane    = "ZNearFar"              ::: V2 GLfloat
type YZProjRatio       = "ZProjRatio"              ::: V2 GLfloat
type YFieldOfView      = "Fovy"                  ::: GLfloat

type YLightPosition      = "lightPosition"      ::: V3 GLfloat
type YLightRadius        = "lightRadius"        ::: V3 GLfloat
type YLightSpecularColor = "lightSpecularColor" ::: V4 GLfloat
type YLightDiffuseColor  = "lightDiffuseColor"  ::: V4 GLfloat
type YLightAmbientColor  = "lightAmbientColor"  ::: V4 GLfloat

type YLightAttributes    = [ YLightPosition, YLightRadius, YLightSpecularColor, YLightDiffuseColor, YLightAmbientColor ]


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

lightPosition :: YLightPosition
lightPosition = Field

lightRadius :: YLightRadius
lightRadius = Field

lightSpecular :: YLightSpecularColor
lightSpecular = Field

lightDiffuse :: YLightDiffuseColor
lightDiffuse = Field

lightAmbient :: YLightAmbientColor
lightAmbient = Field