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
import Yage.Rendering.Transformation

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

mkCameraFps :: (Double, Double) -> Float -> Camera
mkCameraFps (near,far) fov = Camera3D Cam.fpsCamera (CameraPlanes near far) fov

mkCamera2d :: CameraHandle -> (Double, Double) -> Camera
mkCamera2d hnd (near,far) = Camera2D hnd (CameraPlanes near far)

cameraHandle :: Lens' Camera CameraHandle
cameraHandle = lens getHandle setHandle where
    getHandle ( Camera3D hnd _ _ ) = hnd
    getHandle ( Camera2D hnd _   ) = hnd
    
    setHandle ( Camera3D _ planes fov ) hnd = Camera3D hnd planes fov
    setHandle ( Camera2D _ planes     ) hnd = Camera2D hnd planes


cameraPlanes :: Lens' Camera CameraPlanes
cameraPlanes = lens getPlanes setPlanes where
    getPlanes ( Camera3D _ planes _ ) = planes
    getPlanes ( Camera2D _ planes   ) = planes
    
    setPlanes ( Camera3D hnd _ fov ) planes = Camera3D hnd planes fov
    setPlanes ( Camera2D hnd _     ) planes = Camera2D hnd planes


cameraTransformation :: Lens' Camera (Transformation Float)
cameraTransformation = cameraHandle.handleTransformation

cameraLocation :: Lens' Camera (V3 Float)
cameraLocation = cameraTransformation.transPosition

cameraOrientation :: Lens' Camera (Quaternion Float)
cameraOrientation = cameraTransformation.transOrientation

handleTransformation :: Lens' CameraHandle (Transformation Float)
handleTransformation = lens getTransformation setTransformation
    where
    getTransformation Cam.Camera{Cam.orientation, Cam.location} 
        = Transformation location orientation 1
    
    setTransformation hnd Transformation{ _transPosition, _transOrientation } 
        = hnd { Cam.orientation = _transOrientation
              , Cam.location    = _transPosition
              }

cameraMatrix :: Getter Camera (M44 Float)
cameraMatrix = cameraHandle.to Cam.camMatrix 

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

