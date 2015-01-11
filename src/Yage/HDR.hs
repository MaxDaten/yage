
{-# LANGUAGE TemplateHaskell #-}
module Yage.HDR
    ( module Yage.HDR
    , module Camera
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Camera as Camera
import Yage.Transformation
import Data.Data


data HDRCamera = HDRCamera
    { _hdrCameraHandle  :: Camera
    , _hdrExposure      :: Double
    , _hdrExposureBias  :: Double
    , _hdrWhitePoint    :: Double
    , _hdrBloomSettings :: HDRBloomSettings
    } deriving (Show,Eq,Ord,Data,Typeable,Generic)

data HDRBloomSettings = HDRBloomSettings
    { _bloomPreDownsampling :: Int
    , _bloomGaussPasses     :: Int
    , _bloomFactor          :: Double
    , _bloomThreshold       :: Double
    -- ^ ~ 0.5 - 0.8 [Kawase04, Page 29]
    , _bloomWidth           :: Double
    } deriving (Show,Eq,Ord,Data,Typeable,Generic)

makeClassy ''HDRBloomSettings
makeClassyFor "HasHDRCamera" "hdrCamera"
  [ ("_hdrExposure", "exposure")
  , ("_hdrExposureBias", "exposureBias")
  , ("_hdrWhitePoint", "whitePoint")
  , ("_hdrBloomSettings", "bloomSettings")
  ] ''HDRCamera

defaultBloomSettings :: HDRBloomSettings
defaultBloomSettings = HDRBloomSettings
    { _bloomPreDownsampling = 2
    , _bloomGaussPasses     = 7
    , _bloomFactor          = 0.3
    , _bloomThreshold       = 0.6
    , _bloomWidth           = 1.0
    }

defaultHDRCamera :: Camera -> HDRCamera
defaultHDRCamera cam = HDRCamera
    { _hdrCameraHandle  = cam
    , _hdrExposure      = 0.5
    , _hdrExposureBias  = 1.0
    , _hdrWhitePoint    = 0.5
    , _hdrBloomSettings = defaultBloomSettings
    }

instance HasCamera HDRCamera where
    camera = lens _hdrCameraHandle (\s c -> s{_hdrCameraHandle = c})

instance LinearInterpolatable HDRBloomSettings where
    lerp alpha u v =
        u & bloomPreDownsampling .~ v^.bloomPreDownsampling
          & bloomGaussPasses     .~ v^.bloomGaussPasses
          & bloomFactor          .~ lerp alpha (u^.bloomFactor) (v^.bloomFactor)

instance LinearInterpolatable HDRCamera where
    lerp alpha u v =
        u & camera        .~ lerp alpha (u^.camera) (v^.camera)
          & exposure      .~ lerp alpha (u^.exposure) (v^.exposure)
          & exposureBias  .~ lerp alpha (u^.exposureBias) (v^.exposureBias)
          & whitePoint    .~ lerp alpha (u^.whitePoint) (v^.whitePoint)
          & bloomSettings .~ lerp alpha (u^.bloomSettings) (v^.bloomSettings)

instance Default HDRBloomSettings where
    def = defaultBloomSettings
