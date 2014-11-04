{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies #-}
module Yage.Wire.Utils where

import Yage.Prelude
import Control.Wire


spin :: (Num v, Ord v, Monad m, Monoid e) => (v,v) -> v -> Wire s e m (Event v, Event v) v
spin (minV, maxV) x = proc (up, down) ->
    accumHold x -< (\f -> max minV . min maxV . f) <$> mergeL ((+) <$> up) (flip (-) <$> down)


accumHold :: (Monad m, Monoid e) => a -> Wire s e m (Event (a -> a)) a
accumHold x = (hold <|> pure x) . accumE (flip ($)) x
