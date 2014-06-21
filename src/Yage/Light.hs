{-# LANGUAGE GADTs #-}
module Yage.Light where

import Yage.Prelude

import Yage.Transformation
import Linear hiding (lerp)
import qualified Linear (lerp)

data Light where
    Light :: 
        { lightType           :: LightType
        , lightAttribs        :: LightAttributes
        } -> Light

data AmbientLight = AmbientLight (V3 Double)


data LightAttributes where
    LightAttributes ::
        { lAttrColor          :: V4 Double
        , lAttrAttenuation    :: (Double, Double, Double) 
        -- ^ constant, linear, quadric
        , lAttrSpecularExp    :: Double
        -- ^ 0..128: 0 big smooth highlight, 128 tiny hard highlight
        } -> LightAttributes

-- 1 linear, recommended 2 or 3

{--

alpha = distance / radius
damping_factor = 1.0 - pow(alpha,beta)
final_intensity = attenuation(distance) * damping_factor
beta = 1 :: linear

--}

data LightType where
    Pointlight :: LightType
    
    Spotlight ::
        { sLightCutoff :: Double 
        } -> LightType
    
    OmniDirectional :: LightType


instance LinearInterpolatable AmbientLight where
     lerp alpha (AmbientLight u) (AmbientLight v) = AmbientLight $ Linear.lerp alpha u v

instance LinearInterpolatable Light where
    lerp alpha (Light tu attrU) (Light tv attrV) = Light (lerp alpha tu tv) (lerp alpha attrU attrV)

-- FIXME: implement
instance LinearInterpolatable LightAttributes where
    lerp _ u _ = u

instance LinearInterpolatable LightType where
    lerp _ u _ = u