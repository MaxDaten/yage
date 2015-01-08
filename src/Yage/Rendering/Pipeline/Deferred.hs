{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage.Rendering.Pipeline.Deferred
  ( module Pass
  , module Yage.Viewport
  , module RenderSystem
  , DeferredEnvironment
  , DeferredEntity
  , DeferredScene
  , DeferredSky
  , yDeferredLighting
  ) where

import           Yage.Prelude hiding ((</>))
import           Yage.Lens
import           Yage.Vertex hiding (Texture)
import           Yage.Formats.Ygm

import           Data.FileEmbed
import           System.FilePath ((</>))

import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport

import           Yage.Rendering.Pipeline.Deferred.BaseGPass      as Pass
-- import           Yage.Rendering.Pipeline.Deferred.Common         as Pass
-- import           Yage.Rendering.Pipeline.Deferred.DownsamplePass as Pass
-- import           Yage.Rendering.Pipeline.Deferred.GuiPass        as Pass
-- import           Yage.Rendering.Pipeline.Deferred.HDR            as Pass
-- import           Yage.Rendering.Pipeline.Deferred.LightPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass        as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass           as Pass

import           Quine.GL.Sampler
import           Quine.GL.Shader
import           Quine.GL.Types
import           Quine.StateVar

type DeferredEntity      = Entity (RenderData (SVector Word32) (SVector YGMVertex)) (GBaseMaterial Texture)
type DeferredSky         = Entity (RenderData (SVector Word32) (SVector (Position Vec3))) (SkyMaterial Texture)
type DeferredEnvironment = Environment () DeferredSky
type DeferredScene       = Scene DeferredEntity DeferredEnvironment


yDeferredLighting :: (HasViewport a Int, HasScene a DeferredEntity DeferredEnvironment, HasCamera a) => YageResource (RenderSystem a ())
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings $(embedDir "res/glsl") ("/res/glsl"</>)

  baseSampler <- mkBaseSampler
  gBasePass   <- drawGBuffers
  screenQuadPass <- drawRectangle
  skyPass <- drawSky

  return $ do
    val <- ask
    gbuffer <- gBasePass . pure (val^.scene, val^.camera, val^.viewport)
    -- environment & lighting
    envBuffer <- maybe (return gbuffer) (\sky -> skyPass . pure (sky, gbuffer)) (val^.scene.environment.sky)
    -- bring it to screen
    screenQuadPass . pure ([(1,baseSampler,envBuffer^.aBuffer)], val^.viewport)


mkBaseSampler :: YageResource Sampler
mkBaseSampler = throwWithStack $ do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

{--
    let -- renderRes                     = viewport & rectangle %~ fmap (/2.0)
        cam                     = scene^.sceneCamera.hdrCameraHandle
        baseDescr               = Pass.geoPass viewport
        runBasePass             = runRenderPass baseDescr
        baseData                = geoFrameData viewport cam
    in do
    -- render out our geometric attributes (color, normal, ...)
    baseData `runBasePass` ( toGeoEntity cam <$> scene^.sceneEntities )

    -- calculate lighting based on attributes + bloom & apply tone mapping
    hdrTex <- Pass.hdrLightingPass baseDescr viewport scene

    -- rendered gui elements (TODO: should be gamma correct)
    guiTex <- Pass.runGuiPass hdrTex viewport ( scene^.sceneGui )

    -- bring it to the default render target - the screen
    Pass.screenPass viewport [ hdrTex, guiTex ]
--}

-- TODO move orphans instances

instance HasGBaseMaterial mat Texture => HasGBaseMaterial (Entity d mat) Texture where
  gBaseMaterial = materials.gBaseMaterial

instance HasRenderData (Entity (RenderData i v) mat) i v where
  renderData = Yage.Scene.renderData
