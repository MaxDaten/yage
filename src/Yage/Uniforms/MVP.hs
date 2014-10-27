{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Uniforms.MVP where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Data.Vinyl.Universe
import Graphics.Rendering.OpenGL (GLfloat, GLint)

import Yage.Camera
import Yage.Viewport as VP
import Yage.Rendering.Shader



type YProjectionMatrix = "ProjMatrix"           ::: M44 GLfloat
type YViewMatrix       = "ViewMatrix"           ::: M44 GLfloat
type YModelMatrix      = "ModelMatrix"          ::: M44 GLfloat
type YNormalMatrix     = "NormalMatrix"         ::: M33 GLfloat
type YViewToWorldMatrix= "ViewToWorldMatrix"    ::: M33 GLfloat
type YViewToScreen     = "ViewToScreenMatrix"   ::: M44 GLfloat

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

normalMatrix :: SField YNormalMatrix
normalMatrix = SField

projectionMatrix :: SField YProjectionMatrix
projectionMatrix = SField

viewToScreenMatrix :: SField YViewToScreen
viewToScreenMatrix = SField

viewMatrix :: SField YViewMatrix
viewMatrix = SField

modelMatrix :: SField YModelMatrix
modelMatrix = SField

viewToWorldMatrix :: SField YViewToWorldMatrix
viewToWorldMatrix = SField

vpMatrix :: SField YVPMatrix
vpMatrix = SField

mvpMatrix :: SField YMVPMatrix
mvpMatrix = SField

viewportDim :: SField YViewportDim
viewportDim = SField

eyePosition :: SField YEyePosition
eyePosition = SField

zNearPlane :: SField YZNearPlane
zNearPlane = SField

zFarPlane :: SField YZFarPlane
zFarPlane = SField

zNearFarPlane :: SField YZNearFarPlane
zNearFarPlane = SField

zProjRatio :: SField YZProjRatio
zProjRatio = SField

fieldOfView :: SField YFieldOfView
fieldOfView = SField

{--
Utility
--}


perspectiveUniforms :: Viewport Int -> Camera -> Uniforms PerspectiveUniforms
perspectiveUniforms vp cam =
    let near  = realToFrac $ cam^.cameraZNear
        far   = realToFrac $ cam^.cameraZFar
        fov   = realToFrac $ cam^.cameraFov
        projM = projectionMatrix3D near far fov (fromIntegral <$> vp^.viewportRect) :: M44 GLfloat
        viewM = (fmap . fmap) realToFrac (cam^.cameraMatrix)                        :: M44 GLfloat
        vpM   = projM !*! viewM
    in viewMatrix       =: viewM <+>
       -- projectionMatrix =: projM <+>
       vpMatrix         =: vpM


--orthographicUniforms :: Viewport Int -> Camera -> Uniforms '[ YVPMatrix ]
--orthographicUniforms vp cam =
--    let near  = realToFrac $ cam^.cameraZNear
--        far   = realToFrac $ cam^.cameraZFar
--        projM = projectionMatrix2D near far (fromIntegral <$> vp^.viewportRect) :: M44 GLfloat
--        viewM = (fmap . fmap) realToFrac (cam^.cameraMatrix)                    :: M44 GLfloat
--        vpM   = projM !*! viewM
--    in vpMatrix         =: vpM
