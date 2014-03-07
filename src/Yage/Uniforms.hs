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


type YProjectionMatrix = "projection_matrix"     ::: M44 GLfloat
type YViewMatrix       = "view_matrix"           ::: M44 GLfloat
type YModelMatrix      = "model_matrix"          ::: M44 GLfloat
type YNormalMatrix     = "normal_matrix"         ::: M33 GLfloat

type YVPMatrix         = "vp_matrix"             ::: M44 GLfloat
type YMVPMatrix        = "mvp_matrix"            ::: M44 GLfloat

type YAlbedoTex        = "tex_albedo"            ::: GLint
type YNormalTex        = "tex_normal"            ::: GLint
type YTangentTex       = "tex_tangent"           ::: GLint
type YDepthTex         = "tex_depth"             ::: GLint

type YScreenTex        = "screen_texture"        ::: GLint

type YViewportDim      = "viewportDim"           ::: V2 GLint
type YEyePosition      = "eye_position"          ::: V3 GLfloat
type YZNearPlane       = "near"                  ::: GLfloat
type YZFarPlane        = "far"                   ::: GLfloat
type YFieldOfView      = "fovy"                  ::: GLfloat

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

nearPlane :: YZNearPlane
nearPlane = Field

farPlane :: YZFarPlane
farPlane = Field

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