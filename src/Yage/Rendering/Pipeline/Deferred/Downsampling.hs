{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Yage.Rendering.Pipeline.Deferred.Downsampling
  ( Downsampler
  , downsampler
  ) where

import Yage.Prelude

import Quine.GL.Program
import Quine.GL.ProgramPipeline
import Quine.GL.Sampler
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.StateVar
import Yage.GL
import Yage.Lens
import Yage.Math
import Yage.Geometry.D2.Rectangle
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.RenderSystem
import Yage.Rendering.Resources.GL
import Yage.Resources
import Yage.Uniform


#include "definitions.h"


data FragmentShader px = FragmentShader
  { iTexture    :: UniformVar (Texture2D px)
  , iTargetSize :: UniformVar (V2 Int)
  }

-- * Draw To Target
type Downsampler m px = RenderSystem m (Int, Texture2D px) (Texture2D px)

downsampler :: forall px m. (ImageFormat px, MonadResource m) => YageResource (Downsampler m px)
downsampler = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/downsampling.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- liftIO . newIORef =<< createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (IORef (Texture2D px))

  fbo <- glResource

  -- RenderPass
  return $ flip mkStatefulRenderPass (V2 1 1) $ \lastDim (factor, toFilter) -> do
    throwWithStack $ boundFramebuffer RWFramebuffer $= fbo

    let V2 inWidth inHeight = toFilter^.asRectangle.extend
        V2 newWidth newHeight = max 1 (V2 (inWidth `div` factor) (inHeight `div` factor))

    when (lastDim /= V2 newWidth newHeight) $ do
      out <- (\t -> resizeTexture2D t newWidth newHeight) =<< get outputTexture
      outputTexture $= out
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
    iTargetSize $= V2 newWidth newHeight

    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3
    (,V2 newWidth newHeight) <$> get outputTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  smpl <- mkDownsampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog smpl "iTextures[0]")
    <*> fmap (contramap dimensionToTargetSize . SettableStateVar.($=)) (programUniform programUniform4f prog "iTargetSize")
 where
  dimensionToTargetSize (V2 w h) = V4 (fromIntegral w) (fromIntegral h) (recip $ fromIntegral w) (recip $ fromIntegral h)


-- * Samplers

mkDownsampler :: YageResource (UniformSampler2D px)
mkDownsampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
