{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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


-- | planar movement in a 3d space
wasdMovement :: (Real t, Num a) => V2 a -> YageWire t () (V3 a)
wasdMovement (V2 xVel zVel) =
    let pos = V3 ( whileKeyDown Key'D . pure xVel ) ( hold . never ) ( whileKeyDown Key'S . pure zVel ) <&> (<|> 0)
        neg = V3 ( whileKeyDown Key'A . pure xVel ) ( hold . never ) ( whileKeyDown Key'W . pure zVel ) <&> (<|> 0)
    in wire3d $ liftA2 (+) (pos) (negate <$> neg)




wire3d :: V3 (YageWire t a b) -> YageWire t a (V3 b)
wire3d (V3 xw yw zw) = V3 <$> xw <*> yw <*> zw



translationWire :: Num a =>
    M33 a ->
    -- ^ the orthogonal basis
    V3 (YageWire t () a) ->
    -- ^ the signal source for each basis component
    YageWire t () (V3 a)
    -- ^ the resulting translation in the space
translationWire basis = liftA2 (!*) (pure basis) . wire3d



cameraMovement :: Real t =>
    V3 Float ->
    -- ^ starting position
    YageWire t () (V3 Float) ->
    -- ^ the source of the current translation velocity
    YageWire t Camera Camera
    -- ^ updates the camera translation
cameraMovement startPos movementSource =
    proc cam -> do
        trans       <- movementSource    -< ()
        worldTrans  <- integral startPos -< (cam^.cameraOrientation) `rotate` trans
        returnA -< cam & cameraLocation .~ worldTrans

-- mouseVelocity sens 

-- arr ((-mouseSensitivity) * ) . ( velcoityModif . mouseVelocity <|> 0 )
cameraRotation :: (Real t) => 
               YageWire t () (V2 Float) -> 
               YageWire t Camera Camera
cameraRotation velocitySource =
    proc cam -> do
        velV <- velocitySource -< () -- counter clock wise
        x    <- integral 0                   -< velV^._x
        y    <- integrateBounded (-90, 90) 0 -< velV^._y
        returnA -< cam & cameraHandle %~ flip pan x
                       & cameraHandle %~ flip tilt y
