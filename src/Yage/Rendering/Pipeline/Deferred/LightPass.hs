{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Yage.Rendering.Pipeline.Deferred.LightPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Geometry3D
import Yage.Uniforms as U
import Yage.Camera
import Yage.Viewport
import Yage.Scene
import Yage.Transformation
import Yage.Material

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.BaseGPass

