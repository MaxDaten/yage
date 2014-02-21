{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Yage.Scene
    ( module Yage.Scene
    , Cam.rosCamera, Cam.fpsCamera, Cam.camMatrix
    ) where


import           Yage.Prelude
import           Yage.Camera

import           Data.List (length)

import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Viewport
import           Yage.Rendering.Types
import           Yage.Rendering


data Scene geo = Scene
    { _sceneEntities :: [RenderEntity geo]
    , _sceneCamera   :: Camera
    }

makeLenses ''Scene


data SceneView geo = SceneView (Scene geo) ViewportI


class HasScene a geo where
    getScene :: a -> Scene geo

instance (ViableVertex (Vertex geo)) => HasGeoData (Scene geo) geo where
    getGeoEntities s = toRenderEntity <$> s^.sceneEntities

instance (ViableVertex (Vertex lit)) => HasLightData (Scene geo) lit where


instance (ViableVertex (Vertex geo)) => HasGeoData (SceneView geo) geo where
    getGeoEntities (SceneView s _) = getGeoEntities s

instance (ViableVertex (Vertex lit)) => HasLightData (SceneView geo) lit where


emptyRenderScene :: Camera -> Scene geo
emptyRenderScene = Scene []

addRenderable :: (Renderable r geo) => Scene geo -> r -> Scene geo
addRenderable scene r = scene & sceneEntities <>~ [toRenderEntity r]

entitiesCount :: Scene geo -> Int
entitiesCount = length . _sceneEntities

mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera


--fov :: Camera -> Float -> Camera
--fov cam d = cam & cameraProj +~ d

-- | creates the projectiom matrix for the given viewport
-- for Camera2D: create an orthographic matrix with origin at the
-- top left corner of the screen
-- for Camera3D: creates a perspective projection matrix 
cameraProjectionMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                       => Camera -> Viewport a -> M44 a
cameraProjectionMatrix (Camera2D _ planes )     v =
    orthographicMatrix -- 0/0 top left
        ( v^.vpXY._x ) 
        ( v^.vpXY._x + v^.vpSize._x )
        ( v^.vpXY._y )
        ( v^.vpXY._y + v^.vpSize._y )
        ( realToFrac $ planes^.camZNear )
        ( realToFrac $ planes^.camZFar )
cameraProjectionMatrix (Camera3D _ planes fov ) v = 
    Cam.projectionMatrix
        ( realToFrac $ fov )
        ( (v^.vpSize._x) / (v^.vpSize._y) )
        ( realToFrac $ planes^.camZNear )
        ( realToFrac $ planes^.camZFar )

orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                    => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r t b n f = 
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )

