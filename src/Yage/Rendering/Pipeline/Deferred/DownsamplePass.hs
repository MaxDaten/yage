{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.Rendering.Pipeline.Deferred.DownsamplePass where

import Yage.Prelude
import Yage.Lens

import Control.Applicative (liftA)
import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP


import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.Sampler
