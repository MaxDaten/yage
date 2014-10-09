{-# LANGUAGE TemplateHaskell #-}
module Yage.Light where

import           Yage.Lens
import           Yage.Prelude
import           Yage.Math           hiding (lerp)

import qualified Linear              (lerp)
import           Yage.Resources
import           Yage.Transformation

data AmbientLight = AmbientLight (V3 Double)

data LightType =
      Pointlight
        { _pLightPosition :: V3 Double
        -- ^ world position of the light emitter (german: Leuchtmittel)
        , _pLightRadius   :: Double
        -- ^ the total influence distance of the emitter (sphere radius)
        }
    | Spotlight
        { _sLightPosition   :: V3 Double
        -- ^ world position of the spot light emitter
        , _sLightDirection  :: V3 Double
        -- ^ the direction vector in world space
        , _sLightInnerAngle :: Double
        -- ^ inner angle in radians for the area with full intensity
        , _sLightOuterAngle :: Double
        -- ^ outer angle in radians as cut off
        , _sLightRadius     :: Double
        -- ^ maximum distance from `sLightPosition` for light influence
        }
    | Directional
      { _dLightDirection :: V3 Double
      }
    deriving ( Show, Ord, Eq )

makeLenses ''LightType


data Light = Light
    { _lightType      :: LightType
    -- ^ `Poinlight` | `Spotlight`
    , _lightColor     :: V3 Double
    -- ^ tint of the light emitter in linear color dimension
    , _lightIntensity :: Double
    -- ^ the energy in lumen
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



-- | Creates a spotlight
makeSpotlight :: V3 Double
              -- ^ position in world space
              -> V3 Double
              -- ^ target of spotlight in world space
              -> Double
              -- ^ inner angle (degree) for area with max intensity
              -> Double
              -- ^ outer angle (degree) for cut off
              -> V3 Double
              -- ^ light color
              -> Double
              -- ^ intensity (lumen)
              -> Light
              -- ^ constructed spotlight
makeSpotlight position target inner outer color intensity = Light
    { _lightType  = Spotlight { _sLightPosition   = position
                              , _sLightDirection  = normalize $ target - position
                              , _sLightInnerAngle = deg2rad inner
                              , _sLightOuterAngle = deg2rad outer
                              , _sLightRadius     = norm $ target - position
                              }
    , _lightIntensity = intensity
    , _lightColor = color
    }
