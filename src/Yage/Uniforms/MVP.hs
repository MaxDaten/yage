{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Uniforms.MVP where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Vinyl
import Graphics.Rendering.OpenGL (GLfloat, GLint)

import Yage.Camera
import Yage.Viewport as VP
import Yage.Rendering.Shader



type YProjectionMatrix = "ProjMatrix"           ::: M44 GLfloat
type YViewMatrix       = "ViewMatrix"           ::: M44 GLfloat
type YModelMatrix      = "ModelMatrix"          ::: M44 GLfloat
type YNormalMatrix     = "NormalMatrix"         ::: M33 GLfloat

type YVPMatrix         = "VPMatrix"             ::: M44 GLfloat
type YMVPMatrix        = "MVPMatrix"            ::: M44 GLfloat

type YViewportDim      = "ViewportDim"          ::: V2 GLint
type YEyePosition      = "EyePosition"          ::: V3 GLfloat
type YZNearPlane       = "ZNear"                ::: GLfloat
type YZFarPlane        = "ZFar"                 ::: GLfloat
type YZNearFarPlane    = "ZNearFar"             ::: V2 GLfloat
type YZProjRatio       = "ZProjRatio"           ::: V2 GLfloat
type YFieldOfView      = "Fovy"                 ::: GLfloat


type PerspectiveUniforms = [ YViewMatrix, YVPMatrix ]

{--
Fields
--}

normalMatrix :: YNormalMatrix
normalMatrix = Field

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
Utility
--}

perspectiveUniforms :: Viewport Int -> Camera -> Uniforms PerspectiveUniforms
perspectiveUniforms vp cam =
    let near  = realToFrac $ cam^.cameraZNear
        far   = realToFrac $ cam^.cameraZFar
        fov   = realToFrac $ cam^.cameraFov
        projM = projectionMatrix3D near far fov (fromIntegral <$> vp^.viewportRect) :: M44 GLfloat
        viewM = (fmap . fmap) realToFrac (cam^.cameraMatrix)                       :: M44 GLfloat
        vpM   = projM !*! viewM
    in viewMatrix       =: viewM <+>
       vpMatrix         =: vpM


