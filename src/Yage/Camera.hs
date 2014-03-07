{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Camera where


import Yage.Prelude
import Yage.Lens

import qualified Graphics.GLUtil.Camera3D            as Cam

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