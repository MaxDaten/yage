{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
module Yage.Wire.Input where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Yage.UI

import Yage.Wire.Analytic
import Yage.Wire.Types
import Yage.Wire.Event
import Control.Wire

import FRP.Netwire as Netwire

-------------------------------------------------------------------------------
-- State Events

mouseVelocity :: (Real t, RealFloat a) => YageWire t b (V2 a)
mouseVelocity = derivativeF . currentMousePosition


mouseAcceleration :: (Real t, RealFloat a) => YageWire t b (V2 a)
mouseAcceleration = derivativeF . mouseVelocity


keyboardEvent :: (Num t) => YageWire t a (Netwire.Event KeyEvent)
keyboardEvent = go []
    where
    go evQueue = mkSF $ \(Timed _ inputSt) _ ->
        (maybeToEvent $ listToMaybe evQueue, go (drop 1 evQueue ++ inputSt^.keyboardEvents ))

mouseEvent :: (Num t) => YageWire t a (Netwire.Event MouseEvent)
mouseEvent = go []
    where
    go evQueue = mkSF $ \(Timed _ inputSt) _ ->
        (maybeToEvent $ listToMaybe evQueue, go (drop 1 evQueue ++ inputSt^.mouseEvents ))

-- | fires ONCE everytime a `key` is pressed
keyJustPressed :: (Num t) => Key -> YageWire t a (Netwire.Event KeyEvent)
keyJustPressed !key = filterE (keyStateIs key KeyState'Pressed) . keyboardEvent


keyJustReleased :: (Num t) => Key -> YageWire t a (Netwire.Event KeyEvent)
keyJustReleased !key = filterE (\(KeyEvent k _int s _mod) -> key == k && s == KeyState'Released) . keyboardEvent


-- | acts like `id` while `key` is down, inhibits while `key` is up
whileKeyDown :: (Num t) => Key -> YageWire t a a
whileKeyDown !key = proc x -> do
    pressed  <- keyJustPressed key -< ()
    released <- keyJustReleased key -< ()
    between -< (x, pressed, released)

currentMousePosition :: (Real t, Fractional a) => YageWire t b (V2 a)
currentMousePosition = ( pos <$> hold . filterE isMouseMoveEvent . mouseEvent ) <|> 0
    where pos (MouseMoveEvent p) = realToFrac <$> p
          pos _ = error "impossible"


currentInputState :: (Num t) => YageWire t a InputState
currentInputState = mkSF $ \(Timed _ inputState) _ -> (inputState, currentInputState)


-- | a wire for switching between two values 'a' & 'b', starting with 'a'.
--   'trigger' is the event source.
toggle :: Monad m =>
        Wire s e m (Wire s e m a b) (Event (Wire s e m a b))
        -> b -> b -> Wire s e m a b
toggle = go where go trigger a b = dSwitch $ pure a &&& (trigger . pure (go trigger b a))

