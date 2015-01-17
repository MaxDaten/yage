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
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"

data FragmentShader = FragmentShader
  { iTexture :: UniformVar (Texture PixelRGBF)
  }

-- * Draw To Screen

toneMapper :: YageResource (RenderSystem (HDRCamera, Texture PixelRGBF) (Texture PixelRGB8))
toneMapper = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/tonemap.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outTexture <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGB8))
  fbo <- glResource

  -- RenderPass
  return $ do
    (_cam, source) <- ask
    target <- get outTexture
    when (target^.textureDimension /= source^.textureDimension) $ do
      let Texture2D w h = source^.textureDimension
      modifyM outTexture $ \x -> resizeTexture2D x w h
      newtarget <- get outTexture
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

    iTexture $= source

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3

    get outTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  toneSampler <- mkToneSampler
  FragmentShader <$> fmap (contramap Just) (samplerUniform prog toneSampler "iTextures[0]")

-- * Samplers

mkToneSampler :: YageResource (UniformSampler px)
mkToneSampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
