{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Rendering.Pipeline.Deferred.ScreenPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Geometry

import           Yage.Scene
import           Yage.Uniforms                      as U
import           Yage.Viewport                      as VP

import           Yage.Rendering.Pipeline.Deferred.Common
import           Yage.Rendering.Pipeline.Deferred.Sampler
