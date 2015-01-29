{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE Arrows                 #-}

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

import           Yage.Prelude hiding ((</>), cons)
import           Yage.Lens hiding (cons)
import           Yage.Math
import           Yage.Vertex hiding (Texture)
import           Yage.Formats.Ygm


import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.RenderTarget
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport
import           Yage.Material
import           Foreign.Ptr

import           Yage.Rendering.Pipeline.Deferred.BaseGPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.Common         as Pass
import           Yage.Rendering.Pipeline.Deferred.Downsampling   as Pass
-- import           Yage.Rendering.Pipeline.Deferred.GuiPass        as Pass
-- import           Yage.Rendering.Pipeline.Deferred.HDR            as Pass
import           Yage.Rendering.Pipeline.Deferred.Bloom          as Pass
import           Yage.Rendering.Pipeline.Deferred.Tonemap        as Pass
import           Yage.Rendering.Pipeline.Deferred.LightPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass     as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass        as Pass

import           System.FilePath ((</>))
import           Quine.GL.Shader
import           Quine.GL.Types
import           Quine.StateVar
import           Data.Maybe (fromJust)

type DeferredEntity      = Entity (RenderData Word32 YGMVertex) (GBaseMaterial Texture2D)
type DeferredSky         = Entity (RenderData Word32 (Position Vec3)) (SkyMaterial TextureCube)
type DeferredEnvironment = Environment Light DeferredSky
type DeferredScene       = Scene DeferredEntity DeferredEnvironment

maxBloomSamples :: Int
maxBloomSamples = 5

yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, MonadResource m, MonadReader v m, HasViewport v Int)
  => YageResource (RenderSystem m a (Texture2D PixelRGB8))
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings embeddedShaders ("/res/glsl"</>)
  -- throwWithStack $ setupDefaultTexture

  skyPass        <- drawSky

  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  lightPass       <- drawLights
  renderBloom     <- addBloom maxBloomSamples
  tonemapPass     <- toneMapper

  return $ proc input -> do
    mainViewport  <- currentViewport -< ()

    -- render surface attributes for lighting out
    gbufferTarget <- autoResized mkGbufferTarget -< mainViewport
    gbuffer       <- drawGBuffers -< (gbufferTarget, input^.scene, input^.hdrCamera.camera)

    -- environment & lighting
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    lBuffer   <- lightPass -< (input^.scene.environment.lights, radiance, input^.hdrCamera.camera, gbuffer)
    sceneTex  <- skyPass   -< (fromJust $ input^.scene.environment.sky, input^.hdrCamera.camera, lBuffer, gbuffer^.depthBuffer)
    -- bloom pass
    bloomed <- renderBloom -< (0.3,sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    tonemapPass -< (input^.hdrCamera, sceneTex, Just bloomed)
 where
  mkGbufferTarget vp | V2 w h <- vp^.rectangle.extend = GBuffer
    <$> createTexture2D GL_TEXTURE_2D w h
    <*> createTexture2D GL_TEXTURE_2D w h
    <*> createTexture2D GL_TEXTURE_2D w h
    <*> createTexture2D GL_TEXTURE_2D w h


currentViewport :: (MonadReader v m, HasViewport v Int) => RenderSystem m b (Viewport Int)
currentViewport = mkStaticRenderPass $ const (view viewport)
-- TODO move orphans instances

instance HasGBaseMaterial mat Texture2D => HasGBaseMaterial (Entity d mat) Texture2D where
  gBaseMaterial = materials.gBaseMaterial

instance HasSkyMaterial mat TextureCube => HasSkyMaterial (Entity d mat) TextureCube where
  skyMaterial = materials.skyMaterial

instance HasRenderData (Entity (RenderData i v) mat) i v where
  renderData = Yage.Scene.renderData

-- setupDefaultTexture :: MonadIO m => m ()
-- setupDefaultTexture = do
--   throwWithStack $ bindTextures GL_TEXTURE_2D [(0, Just def :: Maybe (Texture PixelRGBA8))]
--   -- throwWithStack $ store black  GL_TEXTURE_2D
--   -- throwWithStack $ upload black GL_TEXTURE_2D 0
--   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
--   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
--   glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 1 1 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr

