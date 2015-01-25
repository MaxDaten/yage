{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TupleSections          #-}
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
import           Yage.Rendering.Pipeline.Deferred.Bloom          as Pass
import           Yage.Rendering.Pipeline.Deferred.Tonemap        as Pass
import           Yage.Rendering.Pipeline.Deferred.LightPass      as Pass
import           Yage.Rendering.Pipeline.Deferred.ScreenPass     as Pass
import           Yage.Rendering.Pipeline.Deferred.SkyPass        as Pass

import           System.FilePath ((</>))
import           Quine.GL.Sampler
import           Quine.GL.Shader
import           Quine.GL.Types
import           Quine.StateVar
import           Data.Maybe (fromJust)

type DeferredEntity      = Entity (RenderData Word32 YGMVertex) (GBaseMaterial Texture)
type DeferredSky         = Entity (RenderData Word32 (Position Vec3)) (SkyMaterial Texture)
type DeferredEnvironment = Environment Light DeferredSky
type DeferredScene       = Scene DeferredEntity DeferredEnvironment

maxBloomSamples :: Int
maxBloomSamples = 5

yDeferredLighting
  :: (HasScene a DeferredEntity DeferredEnvironment, HasHDRCamera a, MonadResource m, MonadReader v m, HasViewport v Int)
  => YageResource (RenderSystem m a ())
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

  return $ proc input -> do
    gbuffer <- gBasePass -< (input^.scene, input^.hdrCamera.camera)

    -- environment & lighting
    let radiance = maybe defaultRadiance (view $ materials.radianceMap.materialTexture) (input^.scene.environment.sky)
    lBuffer   <- lightPass -< (input^.scene.environment.lights, radiance, input^.hdrCamera.camera, gbuffer)
    sceneTex  <- skyPass   -< (fromJust $ input^.scene.environment.sky, input^.hdrCamera.camera, lBuffer, gbuffer^.depthBuffer)
    -- bloom pass
    bloomed <- renderBloom -< (0.5,sceneTex)

    -- tone map from hdr (floating) to discrete Word8
    tonemapped <- tonemapPass -< (input^.hdrCamera, sceneTex, Just bloomed)
    -- bring it to the screen
    screenQuadPass -< [(1.0,baseSampler,tonemapped)]


mkBaseSampler :: YageResource Sampler
mkBaseSampler = throwWithStack $ do
  smpl <- glResource
  samplerParameteri smpl GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri smpl GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri smpl GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri smpl GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return smpl

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
