{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Arrows #-}
module Yage.Wire.Camera where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.UI

import FRP.Netwire

import Yage.Wire.Types
import Yage.Wire.Analytic
import Yage.Wire.Input

import Yage.Camera

cameraMovement :: (Real t) =>
               V3 Float -> MovementKeys -> YageWire t (CameraHandle) (CameraHandle)
cameraMovement startPos _movement =
    let acc         = 2
        toLeft      = -xAxis
        toRight     =  xAxis
        forward     = -zAxis
        backward    =  zAxis
    in proc cam -> do
        leftA      <- pure ( toLeft   ) . whileKeyDown Key'A <|> 0 -< ()
        rightA     <- pure ( toRight  ) . whileKeyDown Key'D <|> 0 -< ()
        forwardA   <- pure ( forward  ) . whileKeyDown Key'W <|> 0 -< ()
        backwardA  <- pure ( backward ) . whileKeyDown Key'S <|> 0 -< ()
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
