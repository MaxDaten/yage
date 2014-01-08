{-# LANGUAGE Arrows #-}
module Yage.Wire
    ( module Yage.Wire
    , module Netwire
    ) where

import Yage.Prelude hiding (id, (.), until)
import Data.Foldable

import Control.Monad (liftM)
import Control.Wire.Unsafe.Event
import Control.Wire.Core as Netwire
import FRP.Netwire as Netwire


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
keyJustPressed key = 
    mkSF $ \(Timed _ inputSt) x ->
        if (key `keyIs` KeyState'Pressed) inputSt
            then (Event x, keyJustPressed key)
            else (NoEvent, keyJustPressed key)


keyJustReleased :: (Num t) => Key -> YageWire t a (Netwire.Event a)
keyJustReleased key = 
    mkSF $ \(Timed _ inputSt) x ->
        if (key `keyIs` KeyState'Released) inputSt
            then (Event x, keyJustReleased key)
            else (NoEvent, keyJustReleased key)


-- | acts like `id` while `key` is down, inhibits while `key` is up
whileKeyDown :: (Num t) => Key -> YageWire t a a
whileKeyDown key = proc a -> do
    down <- keyJustPressed key -< ()
    up <- keyJustReleased key -< ()
    between -< (a, down, up)


currentMousePosition :: (Real t, Fractional b) => YageWire t a (V2 b)
currentMousePosition = mkSF $ \(Timed _ inputSt) _ -> (realToFrac <$> inputSt^.mouse.mousePosition, currentMousePosition)

currentInputState :: (Num t) => YageWire t a InputState
currentInputState = mkSF $ \(Timed _ inputState) _ -> (inputState, currentInputState)

-------------------------------------------------------------------------------
-- Generic Wires


while :: Monoid e => Wire s e m (Netwire.Event a) a
while = mkPure_ $ event (Left mempty) Right


integrateAttenuated :: (Floating a, Ord a, HasTime t s) 
                 => a -> a -> Wire s e m a a
integrateAttenuated a@attenuation x' = mkPure $ \ds dx ->
    let dt = realToFrac (dtime ds)
        x  = a * (x' + dt * dx)
    in x' `seq` ( Right x', integrateAttenuated a x )


integrateBounded :: (Floating a, Ord a, HasTime t s) 
                 => (a, a) -> a -> Wire s e m a a
integrateBounded b@(lower,upper) x' = mkPure $ \ds dx ->
    let dt = realToFrac (dtime ds)
        x  = x' + dt * dx
        n  = min upper . max lower $ x
    in x' `seq` ( Right x', integrateBounded b n )


derivativeF :: (Foldable f, Fractional (f a), RealFloat a, HasTime t s, Monoid e)
            => Wire s e m (f a) (f a)
derivativeF = mkPure $ \_ x -> (Left mempty, loop x)
    where
    loop x' = 
        mkPure $ \ds x ->
            let dt  = realToFrac (dtime ds)
                dx  = (x - x') / dt
                mdx | any isNaN dx       = Right 0
                    | any isInfinite dx  = Left mempty
                    | otherwise          = Right dx
            in mdx `seq` (mdx, loop x)


{--

timeString :: HasTime t s => Wire s e m a String
timeString = fmap (printf "%8.2f") time

countFrame :: (Monad m) => Wire e m a Int
countFrame = countFrom 0 <<< 1


impure :: (Monad m, Functor m) => (a -> m b) -> Wire e m a b
impure f = mkFixM $ \_ x -> Right <$> f x

showW :: (MonadIO m, Functor m, Show a) => Wire e m a a
showW = impure (\x -> liftIO (print x) >> return x )

-- this traces only if value is eval'd
traceW :: (Show a) => Wire s e m a a
traceW = mkPure_ $ \x -> Right (traceShow' x)

--}

--initWith = ((produce . once) <|> empty) . keep


-- | high order wire
-- produces if argument wire produces
-- kepp result of argument wire and keep it forever
-- inhibits forever if argument wire inhibits

{--
produceOnce :: Monad m => Wire e m a b -> Wire e m a b
produceOnce w' = mkGen $ \dt x' -> do
    (mx, _) <- stepWire w' dt x'
    return (mx, mkFixM $ const . const $ return mx)
--}


---------------------------------------------------------------------------------------------------

{--
-- f: channel manipulation function
colorTransformW :: (Double -> Double) -> YageWire ColorChs (Color4 Double)
colorTransformW f = arr (fmap f) . colorW

-- | in: Speed-Vector
--   out: integrated color over time |sin|
colorW :: YageWire ColorChs (Color4 Double)
colorW = arr (uncurryN Color4) . integral_ (0, 0, 0, 0)

clearColorW :: YageWire (Color4 Double) ()
clearColorW = mkFixM $ \_ c -> do
    rConf <- getRenderConfig
    putRenderConfig rConf{ confClearColor = c }
    return $ Right ()
--}

