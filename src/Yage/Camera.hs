{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Camera
  ( Camera(..), HasCamera(..)
  , nearZ, farZ, fovy
  , idCamera
  , cameraMatrix
  , inverseCameraMatrix
  , pitch
  , yaw
  , roll
  ) where


import Yage.Prelude
import Yage.Lens
import Yage.Transformation

import           Data.Data
import           Linear    hiding (lerp, slerp)
import qualified Linear    (lerp, slerp)
import Data.Maybe (fromJust)

data Camera = Camera
  { _cameraFovy        :: !Double
  -- ^ vertical field of view angle in radians
  , _cameraPosition    :: !(V3 Double)
  , _cameraOrientation :: !(Quaternion Double)
  , _cameraNearZ       :: !Double
  , _cameraFarZ        :: !Double
  } deriving (Show,Eq,Ord,Data,Typeable,Generic)


makeClassy ''Camera
makeFields ''Camera

-- | Creates a 'Camera' positioned at the origin
idCamera :: Double -> Double -> Double -> Camera
idCamera fovy' near far = Camera fovy' 0 1 near far

cameraMatrix :: Getter Camera (M44 Double)
cameraMatrix = to g where
  g cam = let con = conjugate (cam^.orientation) in mkTransformation con (rotate con . negate $ cam^.position)

inverseCameraMatrix :: Getter Camera (M44 Double)
inverseCameraMatrix = to g where
  g cam = fromJust $ cam^.cameraMatrix.to inv44 -- mkTransformation (cam^.orientation) (rotate (cam^.orientation) $ cam^.position)
  -- g cam = cam^.transformationMatrix -- mkTransformation (cam^.orientation) (rotate (cam^.orientation) $ cam^.position)

-- | Adjusts the camera view up-and-down by an angle in radians
pitch :: HasCamera c => c -> Double -> c
pitch c theta = c & camera.orientation %~ (*) (axisAngle (V3 1 0 0) theta)

-- | Adjusts the camera view side-to-side by an angle in radians
yaw :: HasCamera c => c -> Double -> c
yaw c theta = c & camera.orientation %~ (*) (axisAngle (V3 0 1 0) theta)

-- | Rotates the camera view along it's view axis by an angle in radians
roll :: HasCamera c => c -> Double -> c
roll c theta = c & camera.orientation %~ (*) (axisAngle (V3 0 0 1) theta)

instance Default Camera where
  def = idCamera (3*pi/8) 0.1 1000

instance HasTransformation Camera Double where
  transformation = lens getter setter where
    getter cam = Transformation (cam^.position) (cam^.orientation) (cam^.scale)
    setter cam trans = cam & position    .~ trans^.position
                           & orientation .~ trans^.orientation
                           & scale       .~ trans^.scale

instance HasScale Camera (V3 Double) where
  scale = lens g s where
    g _cam = V3 1 1 1
    s cam = const cam

instance LinearInterpolatable Camera where
  lerp alpha u v =
    u & cameraFovy .~ (Linear.lerp alpha (V1 $ u^.cameraFovy) (V1 $ v^.cameraFovy))^._x
      & cameraPosition .~ Linear.lerp alpha (u^.cameraPosition) (v^.cameraPosition)
      & cameraOrientation .~ Linear.slerp (u^.cameraOrientation) (v^.cameraOrientation) (realToFrac alpha)
      & cameraNearZ  .~ (Linear.lerp alpha (V1 $ u^.cameraNearZ) (V1 $ v^.cameraNearZ))^._x
      & cameraFarZ   .~ (Linear.lerp alpha (V1 $ u^.cameraFarZ) (V1 $ v^.cameraFarZ))^._x
      & cameraFovy   .~ (Linear.lerp alpha (V1 $ u^.cameraFovy) (V1 $ v^.cameraFovy))^._x
