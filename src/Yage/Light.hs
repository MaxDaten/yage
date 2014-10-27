{-# LANGUAGE TemplateHaskell #-}
module Yage.Light where

import           Yage.Lens
import           Yage.Prelude
import           Yage.Math           hiding (lerp)

import qualified Linear              (lerp)
import           Yage.Transformation

data AmbientLight = AmbientLight (V3 Double)

data LightType =
      Pointlight
        { _pLightPosition   :: V3 Double
        -- ^ world position of the light emitter (german: Leuchtmittel)
        , _pLightRadius     :: Double
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
    | DirectionalLight
      { _dLightDirection    :: V3 Double
      }
    deriving ( Show, Ord, Eq )

makeLenses ''LightType


data Light = Light
    { _lightType      :: LightType
    -- ^ `Poinlight` | `Spotlight` | `DirectionalLight`
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
makeSpotlight position target innerAngle outerAngle color intensity = Light
    { _lightType  = Spotlight
        { _sLightPosition   = position
        , _sLightDirection  = normalize $ target - position
        , _sLightInnerAngle = deg2rad $ clamp innerAngle 0 89.9
        , _sLightOuterAngle = deg2rad $ clamp outerAngle (innerAngle+0.0001) 89.9
        , _sLightRadius     = norm $ target - position
        }
    , _lightIntensity = intensity
    , _lightColor = color
    }


-- | Creates a global directional light
makeDirectionalLight
    :: V3 Double
    -- ^ direction vector in world space (can be unnormalized)
    -> V3 Double
    -- ^ linear emitting color
    -> Double
    -- ^ intensity (lumen)
    -> Light
    -- ^ constructed directional light
makeDirectionalLight direction color intensity = Light
    { _lightType  = DirectionalLight
        { _dLightDirection = normalize $ direction
        }
    , _lightIntensity = intensity
    , _lightColor = color
    }
