{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Arrows              #-}

module Yage.Rendering.Pipeline.Deferred.GaussianBlur
  ( blurPass
  , gaussianSampler
  , linearGaussianSampler
  , LinearSamplingDirection(..)
  ) where

import Yage hiding ((</>), toList, replicateM)
import Yage.Lens hiding (set)
import Yage.GL
import Yage.Uniform
import Data.Data
import Control.Monad (replicateM)
import Control.Arrow
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
  { iToFilter     :: UniformVar (Texture2D px)
  , iAdditive     :: UniformVar (Maybe (Texture2D px))
  , iTargetSize   :: UniformVar (V2 Int)
  , iDirection    :: UniformVar LinearSamplingDirection
  , iUsedTextures :: UniformVar Int
  }

data LinearSamplingDirection = XDirection | YDirection
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

-- Blurring

blurPass :: (ImageFormat px, MonadResource m) => Int -> YageResource (Pass m (Texture2D px) (Texture2D px))
blurPass numSamples = do
  downsamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  return $ proc inTexture -> do
    downsampledTextures <- processDownsamples downsamplers -< [(1::Int,inTexture)]
    processGauss gaussianSamplers -< (map snd downsampledTextures, Nothing)
    -- fromJust <$> foldM (\a (gaussian,(_,t)) -> Just <$> gaussian . pure (t,a)) Nothing (zip gaussianSamplers (reverse downsampledTextures))
    -- returnA -< undefined
 where
  processGauss :: Monad m => [Pass m (Texture2D px, Maybe (Texture2D px)) (Texture2D px)] -> Pass m ([Texture2D px], Maybe (Texture2D px)) (Texture2D px)
  processGauss [] = rmap (fromJust.snd) id
  processGauss (s:ss) = proc ((t:texs), madd) -> do
    out <- s -< (t, madd)
    processGauss ss -< (texs, Just out)

  processDownsamples :: Monad m => [Pass m (Texture2D px) (Texture2D px)] -> Pass m [(Int,Texture2D px)] [(Int,Texture2D px)]
  processDownsamples [] = id
  processDownsamples (s:ss) = proc (t:texs) -> do
    tex <- s -< snd t
    processDownsamples ss -< (2 * fst t,tex):t:texs
    -- let (lastfactor, base) = unsafeLast txs
    -- in fmap ((++) txs . singleton . (2*lastfactor,)) dsampler . pure base

-- * Gaussian Sampler

-- | a gaussian blur filter which operates in two linear filter passes.
gaussianSampler :: (ImageFormat px, MonadResource m) => YageResource (Pass m (Texture2D px, Maybe (Texture2D px)) (Texture2D px))
gaussianSampler = do
  gaussianX <- linearGaussianSampler XDirection
  gaussianY <- linearGaussianSampler YDirection
  return $ proc (inTex,add) -> do
    tx <- gaussianX -< (inTex,Nothing)
    ty <- gaussianY -< (tx,add)
    returnA -< ty

-- ** Linear Sampler Pass

linearGaussianSampler :: forall px m. (ImageFormat px, MonadResource m) => LinearSamplingDirection -> YageResource (RenderSystem m (Texture2D px, Maybe (Texture2D px)) (Texture2D px))
linearGaussianSampler direction = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/gaussian.frag")]
              `compileShaderPipeline` includePaths

  Just (FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

  outputTexture <- liftIO . newIORef =<< createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (IORef (Texture2D px))

  iDirection    $= direction

  fbo <- glResource

  -- RenderPass
  return $ flip mkStatefulRenderPass (V2 1 1) $ \lastDimension (toFilter, mAdd) -> do
    throwWithStack $ boundFramebuffer RWFramebuffer $= fbo

    let V2 inWidth inHeight = toFilter^.asRectangle.extend
        -- V2 newWidth newHeight = V2 (inWidth * upFactor) (inHeight * upFactor)

    when (lastDimension /= V2 inWidth inHeight) $ do
      out <- (\t -> resizeTexture2D t inWidth inHeight) =<< get outputTexture
      outputTexture $= out
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

    (,V2 inWidth inHeight) <$> get outputTexture


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

mkFilterSampler :: YageResource (UniformSampler2D px)
mkFilterSampler = throwWithStack $ sampler2D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkAdditiveSampler :: YageResource (UniformSampler2D px)
mkAdditiveSampler = throwWithStack $ sampler2D 1 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
