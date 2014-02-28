{-# LANGUAGE Arrows #-}
module Yage.Wire
    ( module Yage.Wire
    , module Netwire
    ) where

import Yage.Prelude hiding (id, (.), until, any)
import Yage.Lens

import Data.Foldable

import Control.Wire.Unsafe.Event
import Control.Wire.Core as Netwire
import FRP.Netwire as Netwire hiding (loop)


import Yage.Core.Application
import Yage.Types
import Yage.UI
import Linear


--type ColorChs = (Double, Double, Double, Double)




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
            then (Event x, keyJustPressed key)
            else (NoEvent, keyJustPressed key)


keyJustReleased :: (Num t) => Key -> YageWire t a (Netwire.Event a)
keyJustReleased !key = go
    where go = mkSF $ \(Timed _ inputSt) x ->
            if (key `keyIs` KeyState'Released) inputSt
            then (Event x, keyJustReleased key)
            else (NoEvent, keyJustReleased key)


-- | acts like `id` while `key` is down, inhibits while `key` is up
whileKeyDown :: (Num t) => Key -> YageWire t a a
whileKeyDown !key = go
    where go = mkPure $ \(Timed _ inputSt) x ->
            if (inputSt^.keyboard.keysDown.contains key)
            then (Right x    , go)
            else (Left mempty, go)

 --{-# SCC "-->whileKeyDown" #-} proc a -> do
 --   down <- keyJustPressed key  -< ()
 --   up   <- keyJustReleased key -< ()
 --   between -< a `seq` {-# SCC "whileKeyDown-->" #-} (a, down, up)


currentMousePosition :: (Real t, Fractional b) => YageWire t a (V2 b)
currentMousePosition = go 
    where go = mkSF $ \(Timed _ inputSt) _ -> (realToFrac <$> inputSt^.mouse.mousePosition, go)

currentInputState :: (Num t) => YageWire t a InputState
currentInputState = mkSF $ \(Timed _ inputState) _ -> (inputState, currentInputState)

-------------------------------------------------------------------------------
-- Generic Wires


--while :: Monoid e => Wire s e m (Netwire.Event a) a
--while = mkPure_ $ event (Left mempty) Right


integrateAttenuated :: (Floating a, Ord a, HasTime t s) 
                 => a -> a -> Wire s e m a a
integrateAttenuated a x' = loop
    where 
    loop = mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
            x  = a * (x' + dt * dx)
        in x' `seq` ( Right x', integrateAttenuated a x )


integrateBounded :: (Floating a, Ord a, HasTime t s) 
                 => (a, a) -> a -> Wire s e m a a
integrateBounded (lower,upper) = loop
    where 
    loop x' = mkPure $ \ds dx ->
        let dt = realToFrac (dtime ds)
            x  = x' + dt * dx
            n  = clamp lower upper x
        in x' `seq` ( Right x', loop n )


derivativeF :: (Foldable f, Fractional (f a), RealFloat a, HasTime t s, Monoid e)
            => Wire s e m (f a) (f a)
derivativeF = looping 0
    where
    looping x' = 
        mkPure $ \ds x ->
            let dt  = realToFrac (dtime ds)
                dx  = (x - x') / dt
                mdx | any isNaN dx       = Right 0
                    | any isInfinite dx  = Left mempty
                    | otherwise          = Right dx
            in x' `seq` (mdx, looping x)



