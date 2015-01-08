{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleContexts    #-}
module Yage.Rendering.Pipeline.Deferred.SkyPass
  ( SkyEntity
  , SkyVertexLayout
  , SkyVertex
  , SkyMaterial(..)
  , environmentMap
  , radianceMap
  , drawSky
  ) where

import           Yage.Prelude
import           Yage.Lens

import           Yage.HDR
import           Yage.Material hiding (HasPosition, over, position)
import           Yage.Scene    hiding (Layout)
import           Yage.Uniforms
import           Yage.Vertex
import           Yage.Viewport

import           Yage.Rendering.GL
import           Yage.Rendering.Pipeline.Deferred.BaseGPass (GBuffer)
import           Yage.Rendering.Pipeline.Deferred.Common
import           Yage.Rendering.RenderSystem
import           Yage.Rendering.Resources.GL

import           Quine.GL.Types
import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline

-- * Vertex Attributes
type SkyVertex v = (HasPosition v Vec3)
type SkyVertexLayout v = (HasPosition (HasLayout v) Layout)

type SkyEntity ent i v = (HasTransformation ent Double, HasRenderData ent i v, SkyVertexLayout (Element v), SkyVertex (Element v))

data SkyMaterial t = SkyMaterial
    { _skyEnvironmentMap :: Material MaterialColorAlpha (t PixelRGB8)
    , _skyRadianceMap    :: Material MaterialColorAlpha (t PixelRGB8)
    }

makeLensesWith abbreviatedFields ''SkyMaterial


drawSky :: SkyEntity sky i v => YageResource (RenderSystem (sky, GBuffer) GBuffer)
drawSky = return $ ask <&> snd
