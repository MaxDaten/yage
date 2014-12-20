module Yage.Rendering.Resources.GL.Base
  (
  -- * Generic GL Resources
    glResources
  , glResource
  -- * reexport
  , module Data.Acquire
  ) where

import Yage.Prelude
import Yage.Lens

import Data.Acquire
import Quine.GL.Object

-- | Multiple generic resources
glResources :: (Gen a, Object a) => Int -> Acquire [a]
glResources n = mkAcquire (gens n) (deletes)

-- | Generic resources
glResource :: (Gen a, Object a) => Acquire a
glResource = fmap unsafeHead $ glResources 1
