{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Camera
    ( module Yage.Camera
    , module Cam
    ) where


import Yage.Prelude
import Yage.Lens

import Yage.Rendering.Viewport

import qualified Graphics.GLUtil.Camera3D            as Cam
import Graphics.GLUtil.Camera3D            as Cam (camMatrix)

import Linear


type CameraHandle = Cam.Camera Float

data CameraPlanes = CameraPlanes
    { _camZNear  :: !Double
    , _camZFar   :: !Double
    } deriving (Show)

makeLenses ''CameraPlanes

data Camera =
      Camera3D !CameraHandle !CameraPlanes !Float
    | Camera2D !CameraHandle !CameraPlanes
    deriving (Show)

deriving instance Show CameraHandle


cameraHandle :: Lens' Camera CameraHandle
cameraHandle f (Camera3D hnd planes fov) = fmap (\h -> Camera3D h planes fov) (f hnd)
cameraHandle f (Camera2D hnd planes) = fmap (`Camera2D` planes) (f hnd)

cameraPlanes :: Lens' Camera CameraPlanes
cameraPlanes f (Camera3D hnd planes fov) = fmap (\p -> Camera3D hnd p fov) (f planes)
cameraPlanes f (Camera2D hnd planes) = fmap (Camera2D hnd) (f planes)

cameraLocation :: Lens' CameraHandle (V3 Float)
cameraLocation f (cam@Cam.Camera{Cam.location})= fmap (\l -> cam{Cam.location = l}) (f location)

cameraOrientation :: Lens' CameraHandle (Quaternion Float)
cameraOrientation f (cam@Cam.Camera{Cam.orientation}) = fmap (\o -> cam{Cam.orientation = o}) (f orientation)


-- | for chaining like:
-- >>> cam `dolly` movement
-- >>>     `pan`   turn
-- >>>     `tilt`  tilting
-- 
dolly :: CameraHandle -> V3 Float -> CameraHandle
dolly = flip Cam.dolly

panRad :: CameraHandle -> Float -> CameraHandle
panRad = flip Cam.panRad

pan :: CameraHandle -> Float -> CameraHandle
pan = flip Cam.pan

tiltRad :: CameraHandle -> Float -> CameraHandle
tiltRad = flip Cam.tiltRad

tilt :: CameraHandle -> Float -> CameraHandle
tilt = flip Cam.tilt

rollRad :: CameraHandle -> Float -> CameraHandle
rollRad = flip Cam.rollRad

roll :: CameraHandle -> Float -> CameraHandle
roll = flip Cam.roll




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
        ( realToFrac fov )
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

