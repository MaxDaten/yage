{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Rendering.Pipeline.Deferred.Downsampling
  ( downsampler
  ) where

import Yage hiding ((</>), toList)
import Yage.Lens
import Yage.GL
import Yage.Uniform
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Yage.Rendering.Resources.GL
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.Texture hiding (Texture)
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"


data FragmentShader px = FragmentShader
  { iTexture    :: UniformVar (Texture px)
  , iTargetSize :: UniformVar (V2 Int)
  }


-- * Draw To Screen

downsampler :: forall px. ImageFormat px => YageResource (RenderSystem (Int, Texture px) (Texture px))
downsampler = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/downsampling.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture px))

  lastOutDimensionRef     <- newIORef (V2 1 1)
  fbo <- glResource

  -- RenderPass
  return $ do
    throwWithStack $
      boundFramebuffer RWFramebuffer $= fbo

    (factor, toFilter) <- ask

    let Texture2D inWidth inHeight = toFilter^.textureDimension
        V2 newWidth newHeight = V2 (inWidth `div` factor) (inHeight `div` factor)

    lastOutDimension <- get lastOutDimensionRef
    when (lastOutDimension /= V2 newWidth newHeight) $ do
      lastOutDimensionRef $= V2 newWidth newHeight
      modifyM outputTexture $ \x -> resizeTexture2D x newWidth newHeight
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
    iTargetSize $= V2 newWidth newHeight

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3

    get outputTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  downsampler <- mkDownsampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog downsampler "iTextures[0]")
    <*> fmap (contramap dimensionToTargetSize . SettableStateVar.($=)) (programUniform programUniform4f prog "iTargetSize")
 where
  dimensionToTargetSize (V2 w h) = V4 (fromIntegral w) (fromIntegral h) (recip $ fromIntegral w) (recip $ fromIntegral h)


-- * Samplers

mkDownsampler :: YageResource (UniformSampler px)
mkDownsampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
