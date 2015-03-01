{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Arrows              #-}

module Yage.Rendering.Pipeline.Deferred.Downsampling
  ( Downsampler
  , downsampler
  , batchedDownsampler
  ) where

import Yage.Prelude

import Quine.GL.Program
import Quine.GL.ProgramPipeline
import Quine.GL.Sampler
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.StateVar
import Control.Arrow
import Yage.GL
import Yage.Lens
import Yage.Math
import Yage.Geometry.D2.Rectangle
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.RenderSystem
import Yage.Rendering.RenderTarget
import Yage.Rendering.Resources.GL
import Yage.Resources
import Yage.Uniform


#include "definitions.h"

type DownsamplerIn px = (RenderTarget (Texture2D px), Texture2D px)
type Downsampler m px = Pass (PassRes px) m (DownsamplerIn px) (Texture2D px)

data FragmentShader px = FragmentShader
  { iTexture    :: UniformVar (Texture2D px)
  , iTargetSize :: UniformVar (V2 Int)
  }

data PassRes px = PassRes
  { vao          :: VertexArray
  , pipe         :: Pipeline
  , frag         :: FragmentShader px
  }

-- * Draw To Target

  -- processDownsamples :: Monad m => Downsampler m px -> RenderSystem m ([RenderTarget (Texture2D px)], Texture2D px) [Texture2D px]
batchedDownsampler :: (Functor m, MonadIO m, MonadThrow m, ImageFormat px) => Downsampler m px -> RenderSystem m ([RenderTarget (Texture2D px)],[Texture2D px]) [Texture2D px]
batchedDownsampler p = proc (ta:targets,tex:texs) -> do
  down <- processPass p -< (ta,tex)
  if null targets
    then returnA -< down:tex:texs
    else batchedDownsampler p -< (targets,down:tex:texs)


downsampler :: (ImageFormat px, Functor m, MonadIO m, MonadThrow m) => YageResource (Downsampler m px)
downsampler = Pass <$> passRes <*> pure runRes where
  passRes :: YageResource (PassRes px)
  passRes = do
    emptyvao <- glResource
    boundVertexArray $= emptyvao

    pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
                , $(embedShaderFile "res/glsl/sampling/downsampling.frag")]
                `compileShaderPipeline` includePaths

    Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

    return $ PassRes emptyvao pipeline frag

  -- RenderPass
  runRes :: (Functor m, MonadIO m, MonadThrow m, ImageFormat px) => RenderSystem (ReaderT (PassRes px) m) (DownsamplerIn px) (Texture2D px)
  runRes = mkStaticRenderPass $ \(outTarget, toFilter) -> do
    PassRes{..} <- ask
    throwWithStack $ boundFramebuffer RWFramebuffer $= (outTarget^.framebufferObj)
    glDepthMask GL_TRUE
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW
    -- clear not neccessary
    -- glClearColor 0 1 0 1
    -- glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= vao

    -- set shader uniforms

    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    let FragmentShader{..} = frag
    iTexture $= toFilter
    iTargetSize $= outTarget^.asRectangle.extend

    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3
    return $ outTarget^.renderTarget


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
