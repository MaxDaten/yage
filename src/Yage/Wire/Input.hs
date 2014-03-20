module Yage.Wire.Input where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Yage.UI

import Yage.Wire.Analytic
import Yage.Wire.Types
import Control.Wire

import Control.Wire.Unsafe.Event
import FRP.Netwire as Netwire


-------------------------------------------------------------------------------
-- State Events

mouseVelocity :: (Real t) => YageWire t a (V2 Float)
mouseVelocity = derivativeF . currentMousePosition


mouseAcceleration :: (Real t) => YageWire t a (V2 Float)
mouseAcceleration = derivativeF . mouseVelocity

-- | fires ONCE everytime a `key` is pressed
keyJustPressed :: (Num t) => Key -> YageWire t a (Netwire.Event a)
keyJustPressed !key = go
    where go = mkSF $ \(Timed _ inputSt) x ->
            if (key `keyIs` KeyState'Pressed) inputSt
            then (Event x, go)
            else (NoEvent, go)


keyJustReleased :: (Num t) => Key -> YageWire t a (Netwire.Event a)
keyJustReleased !key = go
    where go = mkSF $ \(Timed _ inputSt) x ->
            if (key `keyIs` KeyState'Released) inputSt
            then (Event x, go)
            else (NoEvent, go)


-- | acts like `id` while `key` is down, inhibits while `key` is up
whileKeyDown :: (Num t) => Key -> YageWire t a a
whileKeyDown !key = go
    where go = mkPure $ \(Timed _ inputSt) x ->
            if (inputSt^.keyboard.keysDown.contains key)
            then (Right x    , go)
            else (Left mempty, go)


currentMousePosition :: (Real t, Fractional b) => YageWire t a (V2 b)
currentMousePosition = go 
    where go = mkSF $ \(Timed _ inputSt) _ -> (realToFrac <$> inputSt^.mouse.mousePosition, go)

currentInputState :: (Num t) => YageWire t a InputState
currentInputState = mkSF $ \(Timed _ inputState) _ -> (inputState, currentInputState)
