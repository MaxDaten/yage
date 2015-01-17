{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}

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

import           Yage.Prelude hiding ((</>), foldM, cons, (++))
import           Yage.Lens hiding (cons)
import           Yage.Vertex hiding (Texture)
import           Yage.Formats.Ygm


import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport
import           Yage.Material

import           Yage.Rendering.Pipeline.Deferred.BaseGPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.Common         as Pass
import           Yage.Rendering.Pipeline.Deferred.Downsampling   as Pass
-- import           Yage.Rendering.Pipeline.Deferred.GuiPass        as Pass
-- import           Yage.Rendering.Pipeline.Deferred.HDR            as Pass
import           Yage.Rendering.Pipeline.Deferred.GaussianBlur   as Pass
import           Yage.Rendering.Pipeline.Deferred.LuminanceFilter   as Pass
import           Yage.Rendering.Pipeline.Deferred.Tonemap        as Pass
import           Yage.Rendering.Pipeline.Deferred.LightPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass     as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass        as Pass

import           System.FilePath ((</>))
import           Control.Monad (foldM)
import           Data.List ((++))
import           Data.Maybe (fromJust)
import           Quine.GL.Sampler
import           Quine.GL.Shader
import           Quine.GL.Types
import           Quine.StateVar

type DeferredEntity      = Entity (RenderData Word32 YGMVertex) (GBaseMaterial Texture)
type DeferredSky         = Entity (RenderData Word32 (Position Vec3)) (SkyMaterial Texture)
type DeferredEnvironment = Environment Light DeferredSky
type DeferredScene       = Scene DeferredEntity DeferredEnvironment

maxBloomSamples :: Int
maxBloomSamples = 5

yDeferredLighting :: (HasViewport a Int, HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a) => YageResource (RenderSystem a ())
yDeferredLighting = do
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $ buildNamedStrings embeddedShaders ("/res/glsl"</>)

  baseSampler <- mkBaseSampler
  gBasePass      <- drawGBuffers
  screenQuadPass <- drawRectangle
  skyPass        <- drawSky
  tonemapPass    <- toneMapper

  defaultRadiance <- textureRes (pure (defaultMaterialSRGB^.materialTexture) :: Cubemap (Image PixelRGB8))
  lightPass       <- drawLights
  renderBloom     <- addBloom maxBloomSamples

  return $ do
    val <- ask
    gbuffer <- gBasePass . pure (val^.scene, val^.hdrCamera.camera, val^.viewport)

    -- environment & lighting
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (val^.scene.environment.sky)
    lBuffer   <- lightPass . pure (val^.scene.environment.lights, radiance, val^.hdrCamera.camera, val^.viewport, gbuffer)
    sceneTex  <- maybe (pure lBuffer)
                       (\skye -> skyPass . pure (skye, val^.hdrCamera.camera, val^.viewport, lBuffer, gbuffer^.depthBuffer)) (val^.scene.environment.sky)
    -- bloom pass
    bloomed <- renderBloom . pure (0.5,sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    tonemapped <- tonemapPass . pure (val^.hdrCamera, sceneTex, Just bloomed)
    -- bring it to the screen
    screenQuadPass . pure ([(1.0,baseSampler,tonemapped)], val^.viewport)


addBloom :: ImageFormat px => Int -> YageResource (RenderSystem (Float,Texture px) (Texture px))
addBloom numSamples = do
  sceneHalf  <- lmap (2,) <$> downsampler
  halfSamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  filterLuma <- luminanceFilter
  return $ do
    (thrshold, inTex) <- ask
    filtered <- filterLuma . fmap (thrshold,) sceneHalf . pure inTex
    downsampledTextures <- reverse <$> foldM processDownsample [(2::Int,filtered)] halfSamplers
    fromJust <$> foldM (\a (gaussian,(_,d)) -> Just <$> gaussian . pure (d,a)) Nothing (zip gaussianSamplers downsampledTextures)
 where
  processDownsample txs dsampler =
    let (lastfactor, base) = unsafeLast txs
    in fmap ((++) txs . singleton . (2*lastfactor,)) dsampler . pure base


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

instance HasSkyMaterial mat Texture => HasSkyMaterial (Entity d mat) Texture where
  skyMaterial = materials.skyMaterial

instance HasRenderData (Entity (RenderData i v) mat) i v where
  renderData = Yage.Scene.renderData
