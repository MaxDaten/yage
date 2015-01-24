{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

module Yage.Rendering.Pipeline.Deferred.GaussianBlur
  ( blurPass
  , gaussianSampler
  , linearGaussianSampler
  , LinearSamplingDirection(..)
  ) where

import Yage hiding ((</>), toList, foldM)
import Yage.Lens hiding (set)
import Yage.GL
import Yage.Uniform
import Data.Data
import Control.Monad (foldM)
import Data.Maybe (fromJust)
import Yage.Rendering.Resources.GL
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.Downsampling

#include "definitions.h"


data FragmentShader px = FragmentShader
  { iToFilter     :: UniformVar (Texture px)
  , iAdditive     :: UniformVar (Maybe (Texture px))
  , iTargetSize   :: UniformVar (V2 Int)
  , iDirection    :: UniformVar LinearSamplingDirection
  , iUsedTextures :: UniformVar Int
  }

data LinearSamplingDirection = XDirection | YDirection
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

-- Blurring

blurPass :: ImageFormat px => Int -> YageResource (RenderSystem (Texture px) (Texture px))
blurPass numSamples = do
  downsamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  return $ do
    inTexture <- ask
    downsampledTextures <- reverse <$> foldM processDownsample [(1::Int,inTexture)] downsamplers
    fromJust <$> foldM (\a (gaussian,(_,t)) -> Just <$> gaussian . pure (t,a)) Nothing (zip gaussianSamplers downsampledTextures)
 where
  processDownsample txs dsampler =
    let (lastfactor, base) = unsafeLast txs
    in fmap ((++) txs . singleton . (2*lastfactor,)) dsampler . pure base

-- * Gaussian Sampler

-- | a gaussian blur filter which operates in two linear filter passes.
gaussianSampler :: ImageFormat px => YageResource (RenderSystem (Texture px, Maybe (Texture px)) (Texture px))
gaussianSampler = do
  gaussianX <- linearGaussianSampler XDirection
  gaussianY <- linearGaussianSampler YDirection
  return $ do
    (inTex, add) <- ask
    tx <- gaussianX . pure (inTex,Nothing)
    ty <- gaussianY . pure (tx,add)
    return ty

-- ** Linear Sampler Pass

linearGaussianSampler :: forall px. ImageFormat px => LinearSamplingDirection -> YageResource (RenderSystem (Texture px, Maybe (Texture px)) (Texture px))
linearGaussianSampler direction = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/gaussian.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture px))

  iDirection    $= direction

  lastDimensionRef     <- newIORef (V2 1 1)
  fbo <- glResource

  -- RenderPass
  return $ do
    throwWithStack $
      boundFramebuffer RWFramebuffer $= fbo

    (toFilter, mAdd) <- ask

    let Texture2D inWidth inHeight = toFilter^.textureDimension
        -- V2 newWidth newHeight = V2 (inWidth * upFactor) (inHeight * upFactor)

    lastDimension <- get lastDimensionRef
    when (lastDimension /= V2 inWidth inHeight) $ do
      lastDimensionRef $= V2 inWidth inHeight
      modifyM outputTexture $ \x -> resizeTexture2D x inWidth inHeight
      out <- get outputTexture
      void $ attachFramebuffer fbo [mkAttachment out] Nothing Nothing

    glDepthMask GL_TRUE
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW

    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= emptyvao

    -- set shader uniforms

    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline


    iToFilter $= toFilter
    iAdditive $= mAdd
    iUsedTextures $= if isJust mAdd then 2 else 1
    iTargetSize $= V2 inWidth inHeight

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3

    get outputTexture


-- * Shader Interface

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  filterSampl <- mkFilterSampler
  additiveSampl <- mkAdditiveSampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog filterSampl "iTextures[0]")
    <*> samplerUniform prog additiveSampl "iTextures[1]"
    <*> fmap (contramap dimensionToTargetSize . SettableStateVar.($=)) (programUniform programUniform4f prog "iTargetSize")
    <*> fmap (contramap dirToVec . SettableStateVar.($=)) (programUniform programUniform2f prog "iDirection")
    <*> fmap (contramap fromIntegral . SettableStateVar.($=)) (programUniform programUniform1i prog "iUsedTextures")
 where
  dimensionToTargetSize (V2 w h) = V4 (fromIntegral w) (fromIntegral h) (recip $ fromIntegral w) (recip $ fromIntegral h)
  dirToVec XDirection = V2 1 0
  dirToVec YDirection = V2 0 1


-- * Samplers

mkFilterSampler :: YageResource (UniformSampler px)
mkFilterSampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkAdditiveSampler :: YageResource (UniformSampler px)
mkAdditiveSampler = throwWithStack $ sampler2D 1 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s