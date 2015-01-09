{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FunctionalDependencies #-}
module Yage.Wire.Utils where

import Yage.Prelude
import Yage.Math
import Control.Wire

-- | a wire to adjust values between a range based on a up and
-- down 'Event'. The 'Event' carries the signed increment/decrement value.
-- 'x': the starting value
-- '(minV,maxV)': the allowed range for the resulting value
spin :: (Num v, Ord v, Monad m, Monoid e) => (v,v) -> v -> Wire s e m (Event v, Event v) v
spin (minV, maxV) x = proc (up, down) ->
    accumHold x -< (\a b -> clamp (a + b) minV maxV) <$> mergeL up down


accumHold :: (Monad m, Monoid e) => a -> Wire s e m (Event (a -> a)) a
accumHold x = (hold <|> pure x) . accumE (flip ($)) x
