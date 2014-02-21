{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Yage.Uniforms where


import Data.Vinyl
import Graphics.Rendering.OpenGL (GLfloat, GLint)
import Linear

type YProjectionMatrix = "projection_matrix" ::: M44 GLfloat
type YViewMatrix       = "view_matrix"       ::: M44 GLfloat
type YVPMatrix         = "vp_matrix"         ::: M44 GLfloat

type YModelMatrix      = "model_matrix"      ::: M44 GLfloat
type YNormalMatrix     = "normal_matrix"     ::: M33 GLfloat
type YScreenTex        = "screen_texture"    ::: GLint

projectionMatrix :: YProjectionMatrix
projectionMatrix = Field

viewMatrix :: YViewMatrix
viewMatrix = Field

vpMatrix :: YVPMatrix
vpMatrix = Field

modelMatrix :: YModelMatrix
modelMatrix = Field

normalMatrix :: YNormalMatrix
normalMatrix = Field

screenTex :: YScreenTex
screenTex = Field 