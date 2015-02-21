{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
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

import           Yage.Rendering.Pipeline.Deferred.BaseGPass       as Pass
import           Yage.Rendering.Pipeline.Deferred.Common          as Pass
import           Yage.Rendering.Pipeline.Deferred.Downsampling    as Pass
-- import           Yage.Rendering.Pipeline.Deferred.GuiPass        as Pass
-- import           Yage.Rendering.Pipeline.Deferred.HDR            as Pass
import           Yage.Rendering.Pipeline.Deferred.PostAmbientPass as Pass
import           Yage.Rendering.Pipeline.Deferred.Bloom           as Pass
import           Yage.Rendering.Pipeline.Deferred.Tonemap         as Pass
import           Yage.Rendering.Pipeline.Deferred.LightPass       as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass         as Pass

import           Yage.Rendering.Pipeline.Voxel.Voxelize           as Pass
import           Yage.Rendering.Pipeline.Voxel.VisualizeVoxel     as Pass

import           Control.Arrow
import           Quine.GL.Shader
import           Quine.StateVar
import           Quine.GL.Types
import           Quine.GL.Texture
import           Data.Maybe (fromJust)

import           Foreign.Ptr
import           Data.Vector.Storable as V hiding (forM_,(++))

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

  --drawGBuffer    <- gPass
  --skyPass        <- drawSky

  --defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  --drawLights      <- lightPass
  --postAmbient     <- postAmbientPass
  --renderBloom     <- addBloom
  tonemapPass     <- toneMapper
  voxelizeScene   <- voxelizePass 128 128 128
  --voxelBuffer     <- genVoxelBuffer 256 256 256
  voxelVis        <- visualizeVoxelPass

  return $ proc input -> do
    mainViewport  <- currentViewport -< ()

    -- render surface attributes for lighting out
    --gbufferTarget <- autoResized mkGbufferTarget           -< mainViewport^.rectangle
    --gBuffer       <- processPassWithGlobalEnv drawGBuffer  -< ( gbufferTarget
                                                              --, input^.scene
                                                              --, input^.hdrCamera.camera )

    -- voxellzation
    (voxelBuffer, pageMask)      <- processPass voxelizeScene    -< input^.scene
    voxelSceneTarget <- autoResized mkVisVoxelTarget -< mainViewport^.rectangle
    voxelScene       <- processPassWithGlobalEnv voxelVis
                         -< ( voxelSceneTarget
                            , VoxelizeScene voxelBuffer
                            , eye4 & _xyz *~ 4
                            , input^.hdrCamera.camera )

    voxelMaskTarget <- autoResized mkVisVoxelTarget -< mainViewport^.rectangle
    voxelMask       <- processPassWithGlobalEnv voxelVis
                         -< (voxelMaskTarget
                            , VoxelPageMask pageMask
                            , eye4 & _xyz *~ 4
                            , input^.hdrCamera.camera )

    -- lighting
    {--
    lBufferTarget <- autoResized mkLightBuffer -< mainViewport^.rectangle
    _lBuffer   <- processPassWithGlobalEnv drawLights  -< ( lBufferTarget
                                                          , input^.scene.environment.lights
                                                          , input^.hdrCamera.camera
                                                          , gBuffer )

    -- ambient
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    post      <- processPassWithGlobalEnv postAmbient -< ( lBufferTarget
                                                         , radiance
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
    tonemapPass -< (input^.hdrCamera.hdrSensor, sceneTex, Just bloomed)
    --}
    tonemapPass -< (input^.hdrCamera.hdrSensor, voxelScene, Just voxelMask)

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

  genVoxelBuffer :: Int -> Int -> Int -> YageResource (Texture3D PixelRGBA8)
  genVoxelBuffer w h d = do
    --let dat = V.replicate (w * h * d * 4) (maxBound :: Word8)
    let dat = V.generate  (w * h * d * 4) (\i -> if i `mod` (w + 1) == 0 then maxBound :: Word8 else minBound)
    tex <- createTexture3D GL_TEXTURE_3D (Tex3D w h d) 1 $ \_ -> do
      glTexParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER GL_NEAREST
      glTexParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER GL_NEAREST

    io $ V.unsafeWith dat $ glTexSubImage3D GL_TEXTURE_3D 0 0 0 0 (fromIntegral w) (fromIntegral h) (fromIntegral d) (pixelFormat (Proxy :: Proxy PixelRGBA8)) (pixelType (Proxy :: Proxy PixelRGBA8)) . castPtr
    boundTexture GL_TEXTURE_3D 0 $= def
    return tex


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

