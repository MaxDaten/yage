{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TypeOperators   #-}
module Yage.Rendering.Pipeline.Deferred.GlareDetectionPass where

import Yage.Prelude
import Yage.Lens

import Control.Applicative (liftA)
import Yage.Scene
import Yage.Uniform as U
import Yage.Viewport as VP

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.Sampler

