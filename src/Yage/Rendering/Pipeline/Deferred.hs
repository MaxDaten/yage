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
  , yDeferredLighting
  ) where

import           Yage.Prelude hiding ((</>), cons)
import           Yage.Lens hiding (cons)
import           Yage.Math

import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.RenderTarget
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport
import           Yage.Material hiding (over)

import           Yage.Rendering.Pipeline.Deferred.BaseGPass       as Pass
import           Yage.Rendering.Pipeline.Deferred.Common          as Pass
import           Yage.Rendering.Pipeline.Deferred.Types           as Pass
import           Yage.Rendering.Pipeline.Deferred.Downsampling    as Pass
-- import           Yage.Rendering.Pipeline.Deferred.GuiPass        as Pass
-- import           Yage.Rendering.Pipeline.Deferred.HDR            as Pass
import           Yage.Rendering.Pipeline.Deferred.PostAmbientPass as Pass
import           Yage.Rendering.Pipeline.Deferred.Bloom           as Pass
import           Yage.Rendering.Pipeline.Deferred.Tonemap         as Pass
import           Yage.Rendering.Pipeline.Deferred.LightPass       as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass         as Pass

import           Yage.Rendering.Pipeline.Voxel.Base

import           Control.Arrow
import           Quine.GL.Shader
import           Data.Maybe (fromJust)


yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, HasDeferredSettings a, DeferredMonad m env)
  => YageResource (RenderSystem m a (Texture2D PixelRGB8))
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings embeddedShaders ((++) "/res/glsl/")
  -- throwWithStack $ setupDefaultTexture

  drawGBuffer    <- gPass
  skyPass        <- drawSky

  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  voxelize        <- voxelizePass 256 256 256
  visVoxel        <- visualizeVoxelPass
  drawLights      <- lightPass
  postAmbient     <- postAmbientPass
  renderBloom     <- addBloom
  tonemapPass     <- toneMapper
  debugOverlay    <- toneMapper

  return $ proc input -> do
    mainViewport  <- sysEnv viewport    -< ()

    -- render surface attributes for lighting out
    gbufferTarget <- autoResized mkGbufferTarget           -< mainViewport^.rectangle
    gBuffer       <- processPassWithGlobalEnv drawGBuffer  -< ( gbufferTarget
                                                              , input^.scene
                                                              , input^.hdrCamera.camera )
    -- voxelize for ambient occlusion
    mVoxelOcclusion <- if input^.deferredSettings.activeVoxelAmbientOcclusion
      then fmap Just voxelize -< input
      else pure Nothing -< ()
    voxelSceneTarget <- autoResized mkVisVoxelTarget -< mainViewport^.rectangle

    -- lighting
    lBufferTarget <- autoResized mkLightBuffer -< mainViewport^.rectangle
    _lBuffer   <- processPassWithGlobalEnv drawLights  -< ( lBufferTarget
                                                          , input^.scene.environment.lights
                                                          , input^.hdrCamera.camera
                                                          , gBuffer )

    -- ambient
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    post      <- processPassWithGlobalEnv postAmbient -< ( lBufferTarget
                                                         , radiance
                                                         , mVoxelOcclusion
                                                         , input^.deferredSettings
                                                         , input^.hdrCamera.camera
                                                         , gBuffer )

    -- sky pass
    skyTarget <- onChange  -< (post, gBuffer^.depthChannel)
    sceneTex <- if isJust $ input^.scene.environment.sky
      then skyPass -< ( fromJust $ input^.scene.environment.sky
                      , input^.hdrCamera.camera
                      , skyTarget
                      )
      else returnA -< post

    -- bloom pass
    bloomed   <- renderBloom -< (input^.hdrCamera.bloomSettings, sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    finalScene <- tonemapPass -< (input^.hdrCamera.hdrSensor, sceneTex, Just bloomed)
    if not (null $ input^.deferredSettings.voxelDebugModes) && isJust mVoxelOcclusion
      then do
        visVoxTex <- processPassWithGlobalEnv visVoxel
                      -< ( voxelSceneTarget
                         , fromJust mVoxelOcclusion
                         , input^.hdrCamera.camera
                         , input^.deferredSettings.voxelDebugModes --[VisualizePageMask] -- [VisualizePageMask] -- [VisualizeSceneVoxel]
                         )
        debugOverlay -< (input^.hdrCamera.hdrSensor, finalScene, Just visVoxTex)
      else returnA -< finalScene

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

-- setupDefaultTexture :: MonadIO m => m ()
-- setupDefaultTexture = do
--   throwWithStack $ bindTextures GL_TEXTURE_2D [(0, Just def :: Maybe (Texture PixelRGBA8))]
--   -- throwWithStack $ store black  GL_TEXTURE_2D
--   -- throwWithStack $ upload black GL_TEXTURE_2D 0
--   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0
--   glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0
--   glTexImage2D GL_TEXTURE_2D 0 GL_RGBA8 1 1 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr

{-

yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, HasDeferredSettings a, DeferredMonad m env)
  => YageResource (RenderSystem m a (Texture2D PixelRGB8))
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings embeddedShaders ((++) "/res/glsl/")
  -- throwWithStack $ setupDefaultTexture

  drawGBuffer    <- gPass
  skyPass        <- drawSky

  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
--  voxelize        <- voxelizePass 256 256 256
--  visVoxel        <- visualizeVoxelPass
  drawLights      <- lightPass
  postAmbient     <- postAmbientPass
  renderBloom     <- addBloom
  tonemapPass     <- toneMapper
--  debugOverlay    <- toneMapper

  return $ proc input -> do
    mainViewport  <- sysEnv viewport    -< ()

    -- render surface attributes for lighting out
    gbufferTarget <- autoResized mkGbufferTarget           -< mainViewport^.rectangle
    gBuffer       <- processPassWithGlobalEnv drawGBuffer  -< ( gbufferTarget
                                                              , input^.scene
                                                              , input^.hdrCamera.camera )

    -- lighting
    lBufferTarget <- autoResized mkLightBuffer -< mainViewport^.rectangle
    _lBuffer   <- processPassWithGlobalEnv drawLights  -< ( lBufferTarget
                                                          , input^.scene.environment.lights
                                                          , input^.hdrCamera.camera
                                                          , gBuffer )

    -- ambient
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    post      <- processPassWithGlobalEnv postAmbient -< ( lBufferTarget
                                                         , radiance
                                                         , Nothing
                                                         , input^.hdrCamera.camera
                                                         , gBuffer )

    -- sky pass
    skyTarget <- onChange  -< (post, gBuffer^.depthChannel)
    sceneTex <- if isJust $ input^.scene.environment.sky
      then skyPass -< ( fromJust $ input^.scene.environment.sky
                      , input^.hdrCamera.camera
                      , skyTarget
                      )
      else returnA -< post

    -- bloom pass
    bloomed   <- renderBloom -< (input^.hdrCamera.bloomSettings, sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    finalScene <- tonemapPass -< (input^.hdrCamera.hdrSensor, sceneTex, Just bloomed)
    returnA -< finalScene
-}
