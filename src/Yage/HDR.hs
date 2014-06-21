{-# LANGUAGE TemplateHaskell #-}
module Yage.HDR
    ( module Yage.HDR
    , module Camera
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Camera as Camera
import Yage.Transformation


data HDRCamera = HDRCamera
    { _hdrCamera       :: Camera
    , _hdrExposure     :: Float
    , _hdrExposureBias :: Float
    } deriving ( Show, Eq, Ord, Generic )

makeLenses ''HDRCamera

instance LinearInterpolatable HDRCamera where
    lerp alpha u v =
        u & hdrCamera       .~ lerp alpha (u^.hdrCamera) (v^.hdrCamera)
          & hdrExposure     .~ lerp alpha (u^.hdrExposure) (v^.hdrExposure)
          & hdrExposureBias .~ lerp alpha (u^.hdrExposureBias) (v^.hdrExposureBias)