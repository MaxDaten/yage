{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Yage.Wire.Analytic where

import Yage.Prelude hiding (any)
import Yage.Math

import Data.Foldable

import Control.Wire

-------------------------------------------------------------------------------
-- Generic Wires


--while :: Monoid e => Wire s e m (Netwire.Event a) a
--while = mkPure_ $ event (Left mempty) Right


integrateAttenuated :: (Floating a, Ord a, HasTime t s) => 
                    a -> a -> Wire s e m a a
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
            n  = clamp upper lower x
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
