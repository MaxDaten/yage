{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE QuasiQuotes        #-}
module Yage.Rendering.Pipeline.Deferred.GeometryPass where

import           Yage.Lens
import           Yage.Math
import           Yage.Prelude

import           Yage.Camera
import qualified Yage.Formats.Ygm                        as YGM
import           Yage.Geometry                           as Geometry
import           Yage.Material
import           Yage.Scene
import           Yage.Uniforms                           as Uniforms
import           Yage.Viewport

import           Yage.Rendering.Pipeline.Deferred.Common

