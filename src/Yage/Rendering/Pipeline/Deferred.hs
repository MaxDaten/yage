{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE PatternGuards          #-}
{-# LANGUAGE RankNTypes             #-}
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
import           Yage.Material hiding (over)
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

type DeferredMonad m env = (MonadResource m, MonadReader env m, HasViewport env Int)

yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, DeferredMonad m env)
  => YageResource (RenderSystem m a (Texture2D PixelRGB8))
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings (embeddedShaders) ((++) "/res/glsl/")
  -- throwWithStack $ setupDefaultTexture

  drawGBuffer    <- gPass
  skyPass        <- drawSky

  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  drawLights      <- lightPass
  renderBloom     <- addBloom
  tonemapPass     <- toneMapper

  return $ proc input -> do
    mainViewport  <- currentViewport -< ()

    -- render surface attributes for lighting out
    gbufferTarget <- autoResized mkGbufferTarget           -< mainViewport^.rectangle
    gBuffer       <- processPassWithGlobalEnv drawGBuffer  -< (gbufferTarget, input^.scene, input^.hdrCamera.camera)

    -- environment & lighting
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    lbufferTarget <- autoResized mkLightBuffer -< mainViewport^.rectangle
    let lightPassInput = (lbufferTarget, input^.scene.environment.lights, radiance, input^.hdrCamera.camera, gBuffer)
    lBuffer   <- processPassWithGlobalEnv drawLights -< lightPassInput

    -- sky pass
    skyTarget <- onChange  -< (lBuffer, gBuffer^.depthChannel)
    sceneTex  <- skyPass   -< (fromJust $ input^.scene.environment.sky, input^.hdrCamera.camera, skyTarget)
    -- bloom pass
    bloomed   <- renderBloom -< (input^.hdrCamera.bloomSettings, sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    tonemapPass -< (input^.hdrCamera, sceneTex, Just bloomed)

 where
  mkGbufferTarget :: Rectangle Int -> YageResource GBuffer
  mkGbufferTarget rect | V2 w h <- rect^.extend = GBuffer
    <$> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1 -- a channel
    <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1 -- b channel
    <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1 -- c channel
    <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1 -- d channel
    <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1 -- depth channel

  mkLightBuffer :: Rectangle Int -> YageResource LightBuffer
  mkLightBuffer rect = let V2 w h = rect^.extend in createTexture2D GL_TEXTURE_2D (Tex2D w h) 1


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

