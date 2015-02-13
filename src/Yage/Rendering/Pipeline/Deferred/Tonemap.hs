{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Rendering.Pipeline.Deferred.Tonemap
  ( toneMapper
  ) where


import Yage
import Yage.Lens
import Yage.GL
import Yage.HDR
import Yage.Uniform
import Yage.Rendering.Resources.GL
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.ProgramPipeline
import Quine.GL.Uniform
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"

data FragmentShader px = FragmentShader
  { iScene     :: UniformVar (Texture2D px)
  , iBloom     :: UniformVar (Maybe (Texture2D px))
  , iHdrSensor :: UniformVar HDRSensor
  }

-- * Draw To Screen

toneMapper :: MonadResource m => YageResource (RenderSystem m (HDRSensor, Texture2D px, Maybe (Texture2D px)) (Texture2D PixelRGB8))
toneMapper = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/tonemap.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outTexture <- liftIO . newIORef =<< createTexture2D GL_TEXTURE_2D (Tex2D 1 1) 1 :: YageResource (IORef (Texture2D PixelRGB8))
  fbo <- glResource

  -- RenderPass
  return $ mkStaticRenderPass $ \(sensor, sceneTex, mBloomTex) -> do
    target <- get outTexture
    when (target^.textureDimension /= sceneTex^.textureDimension) $ do
      let V2 w h = sceneTex^.asRectangle.extend
      newtarget <- (\t -> resizeTexture2D t w h) =<< get outTexture
      outTexture $= newtarget
      void $ attachFramebuffer fbo [mkAttachment newtarget] Nothing Nothing

    throwWithStack $ boundFramebuffer RWFramebuffer $= fbo

    glDisable GL_DEPTH_TEST
    glDepthMask GL_FALSE
    glDepthFunc GL_ALWAYS

    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW

    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= emptyvao

    -- set shader uniforms

    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    iScene $= sceneTex
    iBloom $= mBloomTex
    iHdrSensor $= sensor

    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3

    get outTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  toneSampler <- mkToneSampler
  bloomSampler <- mkBloomSampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog toneSampler "iTextures[0]")
    <*> samplerUniform prog bloomSampler "iTextures[1]"
    <*> hdrSensorUniform prog

hdrSensorUniform :: MonadIO m => Program -> m (UniformVar HDRSensor)
hdrSensorUniform prog = do
  iExposure     <- programUniform programUniform1f prog "Exposure"
  iExposureBias <- programUniform programUniform1f prog "ExposureBias"
  iWhitePoint   <- programUniform programUniform1f prog "WhitePoint"
  return $ SettableStateVar $ \sensor -> do
    iExposure      $= sensor^.exposure.to realToFrac
    iExposureBias  $= sensor^.exposureBias.to realToFrac
    iWhitePoint    $= sensor^.whitePoint.to realToFrac

-- * Samplers

mkToneSampler :: YageResource (UniformSampler2D px)
mkToneSampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkBloomSampler :: YageResource (UniformSampler2D px)
mkBloomSampler = throwWithStack $ sampler2D 1 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
