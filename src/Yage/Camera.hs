{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
module Yage.Camera
    ( Camera(..), HasCamera(..)
    ) where


import Yage.Prelude
import Yage.Lens
import Yage.Transformation

import           Linear                              hiding (lerp, slerp)
import qualified Linear                              (lerp,slerp)
import Data.Data

data Camera = Camera
    { _cameraFovy        :: !Double
    , _cameraPosition    :: !(V3 Double)
    , _cameraOrientation :: !(Quaternion Double)
    , _cameraNearZ       :: !Double
    , _cameraFarZ        :: !Double
    } deriving (Show,Eq,Ord,Data,Typeable,Generic)


makeClassy ''Camera

instance LinearInterpolatable Camera where
  lerp alpha u v =
    u & cameraFovy .~ (Linear.lerp alpha (V1 $ u^.cameraFovy) (V1 $ v^.cameraFovy))^._x
      & cameraPosition .~ Linear.lerp alpha (u^.cameraPosition) (v^.cameraPosition)
      & cameraOrientation .~ Linear.slerp (u^.cameraOrientation) (v^.cameraOrientation) (realToFrac alpha)
      & cameraNearZ  .~ (Linear.lerp alpha (V1 $ u^.cameraNearZ) (V1 $ v^.cameraNearZ))^._x
      & cameraFarZ   .~ (Linear.lerp alpha (V1 $ u^.cameraFarZ) (V1 $ v^.cameraFarZ))^._x
      & cameraFovy   .~ (Linear.lerp alpha (V1 $ u^.cameraFovy) (V1 $ v^.cameraFovy))^._x
