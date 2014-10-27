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
                  V3 Double -> Double -> Double -> Key -> YageWire t (V3 Double) (V3 Double)
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
                    Double -> Double -> V3 Double -> Key -> YageWire t (Quaternion Double) (Quaternion Double)
smoothRotationByKey acc att !axis !key =
    let angleVel    = velocity acc att key
        rot         = axisAngle axis <$> integral 0 . angleVel
    in proc inQ -> do
        rotQ    <- rot -< ()
        returnA -<  inQ * rotQ -- * conjugate rotQ

---------------------------------------------------------------------------------------------------


rotationByVelocity :: (Real t) => V3 Double -> V3 Double -> YageWire t (V2 Double) (Quaternion Double)
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


-- | relative to local space
fpsCameraMovement :: Real t =>
    V3 Double ->
    -- ^ starting position
    YageWire t () (V3 Double) ->
    -- ^ the source of the current translation velocity
    YageWire t Camera Camera
    -- ^ updates the camera translation
fpsCameraMovement startPos movementSource =
    proc cam -> do
        trans       <- movementSource    -< ()
        worldTrans  <- integral startPos -< (cam^.cameraOrientation) `rotate` trans
        returnA -< cam & cameraLocation .~ worldTrans


-- | look around like fps
fpsCameraRotation :: (Real t) =>
               YageWire t () (V2 Double) ->
               YageWire t Camera Camera
fpsCameraRotation velocitySource =
    proc cam -> do
        velV <- velocitySource -< () -- counter clock wise
        x    <- integral 0                   -< velV^._x
        y    <- integrateBounded (-90, 90) 0 -< velV^._y
        returnA -< cam & cameraHandle %~ flip pan x
                       & cameraHandle %~ flip tilt y


-- | rotation about focus point
-- http://gamedev.stackexchange.com/a/20769
arcBallRotation :: ( Real t ) => YageWire t () (V2 Double) -> YageWire t (V3 Double, Camera) Camera
arcBallRotation velocitySource =
    proc (focusPoint, cam) -> do
        let focusToCam = cam^.cameraLocation - focusPoint
        velV <- velocitySource -< ()
        x    <- integral 0                      -< velV^._x
        y    <- integrateBounded (-90, 90) 0    -< velV^._y

        --pos  <- integral 0                  -<
        let rotCam = cam & cameraHandle   %~ flip pan x
                         & cameraHandle   %~ flip tilt y
            pos    = (rotCam^.cameraOrientation) `rotate` focusToCam + focusPoint
        returnA -< rotCam & cameraLocation .~ pos
