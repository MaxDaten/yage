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
    { _hdrCamera        :: Camera
    , _hdrExposure      :: Float
    , _hdrExposureBias  :: Float
    , _hdrWhitePoint    :: Float
    , _hdrBloomSettings :: HDRBloomSettings
    } deriving ( Show, Eq, Ord, Generic )

data HDRBloomSettings = HDRBloomSettings
    { _bloomPreDownsampling :: Int
    , _bloomGaussPasses     :: Int
    , _bloomFactor          :: Float
    , _bloomThreshold       :: Float
    } deriving ( Show, Eq, Ord, Generic )

makeLenses ''HDRBloomSettings
makeLenses ''HDRCamera

defaultBloomSettings :: HDRBloomSettings
defaultBloomSettings = HDRBloomSettings
    { _bloomPreDownsampling = 2
    , _bloomGaussPasses     = 7
    , _bloomFactor          = 0.3
    , _bloomThreshold       = 0.7
    }

defaultHDRCamera :: Camera -> HDRCamera
defaultHDRCamera camera = HDRCamera
    { _hdrCamera        = camera
    , _hdrExposure      = 0.5
    , _hdrExposureBias  = 1.0
    , _hdrWhitePoint    = 0.5
    , _hdrBloomSettings = defaultBloomSettings
    }

instance LinearInterpolatable HDRBloomSettings where
    lerp alpha u v =
        u & bloomPreDownsampling .~ v^.bloomPreDownsampling
          & bloomGaussPasses     .~ v^.bloomGaussPasses
          & bloomFactor          .~ lerp alpha (u^.bloomFactor) (v^.bloomFactor)

instance LinearInterpolatable HDRCamera where
    lerp alpha u v =
        u & hdrCamera        .~ lerp alpha (u^.hdrCamera) (v^.hdrCamera)
          & hdrExposure      .~ lerp alpha (u^.hdrExposure) (v^.hdrExposure)
          & hdrExposureBias  .~ lerp alpha (u^.hdrExposureBias) (v^.hdrExposureBias)
          & hdrWhitePoint    .~ lerp alpha (u^.hdrWhitePoint) (v^.hdrWhitePoint)
          & hdrBloomSettings .~ lerp alpha (u^.hdrBloomSettings) (v^.hdrBloomSettings)

instance Default HDRBloomSettings where
    def = defaultBloomSettings
