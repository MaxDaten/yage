{-# LANGUAGE TemplateHaskell #-}
module Yage.Light where

import Yage.Prelude
import Yage.Lens

import Yage.Transformation
import Yage.Resources
import Linear hiding (lerp)
import qualified Linear (lerp)

data AmbientLight = AmbientLight (V3 Double)

data LightType =
      Pointlight
      { _pLightPosition       :: V3 Double
      , _pLightRadius         :: Double }
    | Spotlight
      { _sLightPosition       :: V3 Double
      , _sLightCutoff         :: Double }
    | OmniDirectional
      { _odLightDirection     :: V3 Double }
    deriving ( Show, Ord, Eq )

makeLenses ''LightType


data Light = Light
    { _lightType           :: LightType
    , _lightColor          :: V3 Double
    , _lightIntensity      :: Double
    } deriving ( Show, Ord, Eq )

makeLenses ''Light



instance LinearInterpolatable AmbientLight where
     lerp alpha (AmbientLight u) (AmbientLight v) = AmbientLight $ Linear.lerp alpha u v

instance LinearInterpolatable Light where
    lerp alpha (Light tu colorU intensityU) (Light tv colorV intensityV) =
        Light (lerp alpha tu tv)
              (Linear.lerp alpha colorU colorV)
              ((Linear.lerp alpha (V1 intensityU) (V1 intensityV))^._x)

instance LinearInterpolatable LightType where
    lerp _ u _ = u

instance HasResources vert Light Light where
    requestResources = return
