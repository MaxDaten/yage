{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Camera
    ( module Yage.Camera
    ) where


import Yage.Prelude
import Yage.Lens

import Yage.Transformation

import qualified Graphics.GLUtil.Camera3D            as Cam

import           Linear                              hiding (lerp, slerp)
import qualified Linear                              (lerp, slerp)


type CameraHandle = Cam.Camera Float
deriving instance Show CameraHandle
deriving instance Eq CameraHandle
deriving instance Ord CameraHandle

data CameraPlanes = CameraPlanes
    { _camZNear  :: !Double
    , _camZFar   :: !Double
    } deriving ( Show, Eq, Ord, Generic )

makeLenses ''CameraPlanes

data Camera = Camera
    { _cameraHandle :: !CameraHandle
    , _cameraPlanes :: !CameraPlanes
    , _cameraFov    :: !Float
    } deriving ( Show, Eq, Ord, Generic )

makeLenses ''Camera


mkCameraFps :: Float -> (Double, Double) -> Transformation Float -> Camera
mkCameraFps fov (near,far) trans =
    Camera Cam.fpsCamera (CameraPlanes near far) fov
        & cameraTransformation .~ trans



cameraTransformation :: Lens' Camera (Transformation Float)
cameraTransformation = cameraHandle.handleTransformation

cameraLocation :: Lens' Camera (V3 Float)
cameraLocation = cameraTransformation.transPosition

cameraOrientation :: Lens' Camera (Quaternion Float)
cameraOrientation = cameraTransformation.transOrientation

cameraForward :: Lens' Camera (V3 Float)
cameraForward = lens getter setter where
    getter cam = Cam.forward $ cam^.cameraHandle
    setter cam fwd = cam & cameraHandle %~ \hnd -> hnd{ Cam.forward = fwd }

cameraUpward :: Lens' Camera (V3 Float)
cameraUpward = lens getter setter where
    getter cam = Cam.upward $ cam^.cameraHandle
    setter cam up = cam & cameraHandle %~ \hnd -> hnd{ Cam.upward = up }

handleTransformation :: Lens' CameraHandle (Transformation Float)
handleTransformation = lens getter setter where
    getter Cam.Camera{Cam.orientation, Cam.location}
        = Transformation location orientation 1

    setter hnd Transformation{ _transPosition, _transOrientation }
        = hnd { Cam.orientation = _transOrientation
              , Cam.location    = _transPosition
              }


cameraZNear :: Lens' Camera Double
cameraZNear = cameraPlanes.camZNear

cameraZFar :: Lens' Camera Double
cameraZFar = cameraPlanes.camZFar


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




instance LinearInterpolatable CameraHandle where
    lerp alpha u v = let linLerp = Linear.lerp (realToFrac alpha) in
        Cam.Camera
            { Cam.forward       = linLerp      (Cam.forward u)      (Cam.forward v)
            , Cam.upward        = linLerp      (Cam.upward u)       (Cam.upward v)
            , Cam.rightward     = linLerp      (Cam.rightward u)    (Cam.rightward v)
            , Cam.orientation   = Linear.slerp (Cam.orientation u)  (Cam.orientation v) (realToFrac alpha)
            , Cam.location      = linLerp      (Cam.location u)     (Cam.location v)
            }

instance LinearInterpolatable CameraPlanes where
    lerp alpha u v = let linLerp = Linear.lerp (realToFrac alpha) in
        CameraPlanes
            { _camZFar  = (linLerp (V1 $ u^.camZFar) (V1 $ v^.camZFar))^._x
            , _camZNear = (linLerp (V1 $ u^.camZNear) (V1 $ v^.camZNear))^._x
            }

instance LinearInterpolatable Camera where
    lerp alpha u v = u & cameraHandle .~ lerp alpha (u^.cameraHandle) (v^.cameraHandle)
                       & cameraPlanes .~ lerp alpha (u^.cameraPlanes) (v^.cameraPlanes)
                       & cameraFov    .~ lerp alpha (u^.cameraFov)    (v^.cameraFov)


