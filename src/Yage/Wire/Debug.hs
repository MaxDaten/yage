{-# LANGUAGE Arrows #-}
module Yage.Wire.Debug where

import Yage.Prelude
import Control.Wire


-- | traces the input signal with the (local) time stamp
traceW :: (HasTime t s, Show a, Show t, Real t, MonadIO m) => Wire s e m a a
traceW = proc x -> do
  t <- time -< ()
  printW -< (t, x)
  where
  printW = mkGen_ $ \(t,x) -> printTF "{}:{}\n" (fixed 4 t, Shown x) >> return (Right x)
