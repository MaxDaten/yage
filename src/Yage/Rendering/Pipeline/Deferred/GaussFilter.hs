{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE QuasiQuotes    #-}
module Yage.Rendering.Pipeline.Deferred.GaussFilter where

import Yage.Prelude
import Yage.Lens

import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.Sampler
