{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Rendering.Pipeline.Deferred.Gaussian
  ( gaussianSampler
  , linearGaussianSampler
  , LinearSamplingDirection(..)
  ) where

import Yage hiding ((</>), toList)
import Yage.Lens hiding (set)
import Yage.GL
import Yage.Uniform
import Data.Foldable (traverse_)
import Data.Data
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
  { iToFilter     :: UniformVar (Texture px)
  , iAdditive     :: UniformVar (Texture px)
  , iTargetSize   :: UniformVar (V2 Int)
  , iDirection    :: UniformVar (Vec2)
  , iUsedTextures :: UniformVar Int
  }

data LinearSamplingDirection = XDirection | YDirection
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)


-- * Draw To Screen

-- | a gaussian blur filter which operates in two linear filter passes.
gaussianSampler :: ImageFormat px => YageResource (RenderSystem (Int, Texture px, Maybe (Texture px)) (Texture px))
gaussianSampler = do
  gaussianX <- linearGaussianSampler XDirection
  gaussianY <- linearGaussianSampler YDirection
  return $ do
    (scale, inTex, add) <- ask
    tx <- gaussianX . pure (1,inTex,Nothing)
    ty <- gaussianY . pure (1,tx,add)
    return ty

linearGaussianSampler :: forall px. ImageFormat px => LinearSamplingDirection -> YageResource (RenderSystem (Int, Texture px, Maybe (Texture px)) (Texture px))
linearGaussianSampler direction = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/gaussian.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture px))

  lastDimensionRef     <- newIORef (V2 1 1)
  fbo <- glResource

  -- RenderPass
  return $ do
    throwWithStack $
      boundFramebuffer RWFramebuffer $= fbo

    (upFactor, toFilter, mAdd) <- ask

    let Texture2D inWidth inHeight = toFilter^.textureDimension
        V2 newWidth newHeight = V2 (inWidth * upFactor) (inHeight * upFactor)

    lastOutDimension <- get lastDimensionRef
    when (lastOutDimension /= V2 newWidth newHeight) $ do
      lastDimensionRef $= V2 newWidth newHeight
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


    iToFilter $= toFilter
    traverse_ (set iAdditive) mAdd
    iUsedTextures $= if isJust mAdd then 2 else 1
    iTargetSize   $= V2 newWidth newHeight
    iDirection    $= if direction == XDirection then V2 1 0 else V2 0 1

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3

    get outputTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  smpl <- mkSampler
  FragmentShader
    <$> samplerUniform prog smpl "iTextures[0]"
    <*> samplerUniform prog smpl "iTextures[1]"
    <*> fmap (contramap dimensionToTargetSize . SettableStateVar.($=)) (programUniform programUniform4f prog "iTargetSize")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform2f prog "iDirection")
    <*> fmap (contramap fromIntegral . SettableStateVar.($=)) (programUniform programUniform1i prog "iUsedTextures")
 where
  dimensionToTargetSize (V2 w h) = V4 (fromIntegral w) (fromIntegral h) (recip $ fromIntegral w) (recip $ fromIntegral h)


-- * Samplers

mkSampler :: YageResource (UniformSampler px)
mkSampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
