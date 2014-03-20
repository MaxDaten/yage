{-# LANGUAGE Arrows #-}
module Yage.Wire.Movement where

import Yage.Prelude
import Yage.Lens

import Yage.Math
import Yage.UI

import Yage.Wire.Types
import Yage.Wire.Analytic
import Yage.Wire.Input

import FRP.Netwire as Netwire hiding (loop)

{--
-- Movement
--}


smoothTranslation :: (Real t) => 
                  V3 Float -> Float -> Float -> Key -> YageWire t (V3 Float) (V3 Float)
smoothTranslation dir acc att key =
    let trans = integral 0 . arr (signorm dir ^*) . velocity acc att key
    in proc inTransV -> do
        transV <- trans -< ()
        returnA -< inTransV + transV 


velocity :: (Floating b, Ord b, Real t) 
         => b -> b -> Key -> YageWire t a b
velocity !acc !att !trigger = 
    integrateAttenuated att 0 . (pure acc . whileKeyDown trigger <|> 0)


smoothRotationByKey :: (Real t) => 
                    Float -> Float -> V3 Float -> Key -> YageWire t (Quaternion Float) (Quaternion Float)
smoothRotationByKey acc att !axis !key = 
    let angleVel    = velocity acc att key
        rot         = axisAngle axis <$> integral 0 . angleVel
    in proc inQ -> do
        rotQ    <- rot -< ()
        returnA -<  inQ * rotQ -- * conjugate rotQ

---------------------------------------------------------------------------------------------------


rotationByVelocity :: (Real t) => V3 Float -> V3 Float -> YageWire t (V2 Float) (Quaternion Float)
rotationByVelocity !xMap !yMap =
    let applyOrientations   = arr (axisAngle xMap . (^._x)) &&& arr (axisAngle yMap . (^._y))
        combineOrientations = arr (\(!qu, !qr) -> qu * qr)
    in combineOrientations . applyOrientations . integral 0
