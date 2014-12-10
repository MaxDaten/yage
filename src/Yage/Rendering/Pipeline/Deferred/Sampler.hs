{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE KindSignatures   #-}
module Yage.Rendering.Pipeline.Deferred.Sampler where

import Yage.Prelude
import Yage.Lens

import Yage.Viewport
import Yage.Geometry
import Yage.Uniforms as U hiding (ShaderSource)

import Yage.Rendering.Pipeline.Deferred.Common
