{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Light where

import           Yage.Lens
import           Yage.Prelude
import           Yage.Math           hiding (lerp)

import qualified Linear              (lerp)
import           Data.Data
import           Yage.Transformation

data AmbientLight = AmbientLight (V3 Double)
  deriving (Show,Read,Ord,Eq,Typeable,Data,Generic)

data LightType =
    Pointlight
  | DirectionalLight
  | Spotlight
      { _innerAngle :: !Double
      -- ^ inner angle in radians for the area with full intensity
      , _outerAngle :: !Double
      -- ^ outer angle in radians as cut off
      }
  deriving (Show,Read,Ord,Eq,Typeable,Data,Generic)

makeLenses ''LightType


data Light = Light
    { _lightType            :: !LightType
    -- ^ `Poinlight` | `Spotlight` | `DirectionalLight`
    , _lightTransformation  :: !(Transformation Double)
    -- ^ position, direction (orientation) and radius (scale)
    , _lightColor           :: !(V3 Double)
    -- ^ tint of the light emitter in linear color dimension
    , _lightIntensity       :: !Double
    -- ^ the energy in lumen
    } deriving (Show,Read,Ord,Eq,Typeable,Data,Generic)

makeLenses ''Light

instance HasTransformation Light Double where
  transformation = lightTransformation

instance HasPosition Light (V3 Double) where
  position = lightTransformation.position

instance HasOrientation Light (Quaternion Double) where
  orientation = lightTransformation.orientation

instance HasScale Light (V3 Double) where
  scale = lightTransformation.scale



instance LinearInterpolatable AmbientLight where
     lerp alpha (AmbientLight u) (AmbientLight v) = AmbientLight $ Linear.lerp alpha u v

instance LinearInterpolatable Light where
    lerp alpha (Light tu transU colorU intensityU) (Light tv transV colorV intensityV) =
        Light (lerp alpha tu tv)
              (lerp alpha transU transV)
              (Linear.lerp alpha colorU colorV)
              ((Linear.lerp alpha (V1 intensityU) (V1 intensityV))^._x)

instance LinearInterpolatable LightType where
    lerp _ u _ = u



-- | Creates a spotlight
makeSpotlight
  :: V3 Double
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
makeSpotlight pos target innerAngle outerAngle color intensity = Light
  { _lightType  = Spotlight
    { _innerAngle = innerRad
    , _outerAngle = outerRad
    }
  , _lightTransformation = idTransformation
      & position    .~ pos
      & orientation .~ lookAtQ worldpace direction
      & scale       .~ V3 basisRadius radius basisRadius
  , _lightIntensity = intensity
  , _lightColor = color
  }
 where
  innerRad = deg2rad $ clamp innerAngle 0 89.9
  outerRad = deg2rad $ clamp outerAngle (innerAngle+0.0001) 89.9
  radius = norm $ target - pos
  direction = normalize $ target - pos
  half = outerRad / 2.0
  basisRadius = radius * sin half / sin (pi / 2.0 - half)
  worldpace = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

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
    , _lightTransformation = idTransformation & orientation .~ lookAtQ worldpace (normalize direction)
    , _lightIntensity = intensity
    , _lightColor = color
    }
 where
  worldpace = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- | Creates a point omnidirectional light
makePointlight
  :: V3 Double
  -- ^ position in world space
  -> Double
  -- ^ radius
  -> V3 Double
  -- ^ linear emitting color
  -> Double
  -- ^ intensity (lumen)
  -> Light
  -- ^ constructed point light
makePointlight pos radius color intensity = Light
    { _lightType           = Pointlight
    , _lightTransformation = idTransformation & position .~ pos & scale .~ pure radius
    , _lightIntensity      = intensity
    , _lightColor          = color
    }
