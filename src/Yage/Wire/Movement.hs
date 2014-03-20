{-# LANGUAGE Arrows #-}
module Yage.Wire.Movement where

import Yage.Prelude
import Yage.Lens

import Yage.Math
import Yage.UI

import Yage.Wire.Types
import Yage.Wire.Analytic
import Yage.Wire.Input

import FRP.Netwire as Netwire hiding (loop, left, right)

import Yage.Camera
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



cameraMovement :: (Real t) =>
               V3 Float -> MovementKeys -> YageWire t (CameraHandle) (CameraHandle)
cameraMovement startPos (MovementKeys left right forw backw) =
    let acc         = 2
        toLeft      = -xAxis
        toRight     =  xAxis
        forward     = -zAxis
        backward    =  zAxis
    in proc cam -> do
        leftA      <- pure ( toLeft   ) . whileKeyDown left  <|> 0 -< ()
        rightA     <- pure ( toRight  ) . whileKeyDown right <|> 0 -< ()
        forwardA   <- pure ( forward  ) . whileKeyDown forw  <|> 0 -< ()
        backwardA  <- pure ( backward ) . whileKeyDown backw <|> 0 -< ()
        let trans  = leftA + rightA + forwardA + backwardA
            r      = (cam^.cameraOrientation)
        transV  <- integral startPos -< acc *^ normalize $ r `rotate` trans 
        returnA -< (cam & cameraLocation .~ transV)


cameraRotation :: (Real t) => 
               V2 Float -> YageWire t (CameraHandle) (CameraHandle)
cameraRotation mouseSensitivity =
    proc cam -> do
        velV <- arr ((-mouseSensitivity) * ) . (whileKeyDown Key'LeftShift . mouseVelocity <|> 0) -< () -- counter clock wise
        x    <- integral 0                   -< velV^._x
        y    <- integrateBounded (-90, 90) 0 -< velV^._y
        returnA -< cam `pan`  x
                       `tilt` y
