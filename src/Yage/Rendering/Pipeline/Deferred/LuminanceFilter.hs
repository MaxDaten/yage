{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Yage.Rendering.Pipeline.Deferred.LuminanceFilter
  ( luminanceFilter
  ) where

import Yage hiding ((</>), toList)
import Yage.Lens
import Yage.GL
import Yage.Uniform
import Yage.Rendering.Resources.GL
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"


data FragmentShader px = FragmentShader
  { iTexture          :: UniformVar (Texture px)
  , iLuminanceCutoff  :: UniformVar Float
  }


-- * Draw To Screen

luminanceFilter :: forall px m. (MonadResource m, ImageFormat px) => YageResource (RenderSystem m (Float,Texture px) (Texture px))
luminanceFilter = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/luminancefilter.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture px))

  fbo <- glResource

  -- RenderPass
  return $ flip mkStatefulRenderPass (V2 1 1) $ \lastDim (cutoff, toFilter) -> do
    throwWithStack $ boundFramebuffer RWFramebuffer $= fbo

    let Texture2D inWidth inHeight = toFilter^.textureDimension
    when (lastDim /= V2 inWidth inHeight) $ do
      modifyM outputTexture $ \x -> resizeTexture2D x inWidth inHeight
      out <- get outputTexture
      void $ attachFramebuffer fbo [mkAttachment out] Nothing Nothing

    glDepthMask GL_TRUE
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW
    -- clear not neccessary
    -- glClearColor 0 1 0 1
    -- glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= emptyvao

    -- set shader uniforms

    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline


    iTexture $= toFilter
    iLuminanceCutoff $= cutoff

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3

    (,V2 inWidth inHeight) <$> get outputTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  s <- mkFiltersampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog s "iTextures[0]")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform1f prog "iLuminanceCutoff")


-- * Samplers

mkFiltersampler :: YageResource (UniformSampler px)
mkFiltersampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
