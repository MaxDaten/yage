{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}

module Yage.Rendering.Pipeline.Deferred.GaussianBlur
  ( blurRenderSystem
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
import Yage.Rendering.RenderTarget
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

blurRenderSystem :: (ImageFormat px, MonadResource m) => Int -> YageResource (RenderSystem m (Texture2D px) (Texture2D px))
blurRenderSystem numSamples = do
  downsamplers   <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussPass      <- dimap (\((a,b),c)->(a,b,c)) Just <$> gaussianSampler
  return $ proc inTexture -> do
    downsampledTextures <- processDownsamples downsamplers -< [(1::Int,inTexture)]
    targets             <- mapA (autoResized mkTarget)     -< mapped %~ (view $ _2.asRectangle) $ downsampledTextures
    let gaussFoldingInput = (zipWith (\t (_,d) -> (t,d)) targets downsampledTextures, Nothing)
    fromJust <$> foldA gaussPass -< gaussFoldingInput
 where
  mkTarget rect = let V2 w h = rect^.extend in createTexture2D GL_TEXTURE_2D w h

  processDownsamples :: Monad m => [RenderSystem m (Texture2D px) (Texture2D px)] -> RenderSystem m [(Int,Texture2D px)] [(Int,Texture2D px)]
  processDownsamples [] = id
  processDownsamples (s:ss) = proc (t:texs) -> do
    tex <- s -< snd t
    processDownsamples ss -< (2 * fst t,tex):t:texs
    -- let (lastfactor, base) = unsafeLast txs
    -- in fmap ((++) txs . singleton . (2*lastfactor,)) dsampler . pure base

-- * Gaussian Sampler

-- | a gaussian blur filter which operates in two linear filter RenderSystemes.
gaussianSampler :: (ImageFormat px, MonadResource m) => YageResource (RenderSystem m (LinearGaussianIn px) (Texture2D px))
gaussianSampler = do
  gaussianX <- linearGaussianSampler XDirection
  gaussianY <- linearGaussianSampler YDirection
  return $ proc (outTarget,inTex,add) -> do
    xTarget <- autoResized mkXTarget -< outTarget^.asRectangle
    tx <- processPass gaussianX -< (xTarget,inTex,Nothing)
    ty <- processPass gaussianY -< (outTarget,tx,add)
    returnA -< ty
 where
  mkXTarget rect = let V2 w h = rect^.extend in createTexture2D GL_TEXTURE_2D w h

-- ** Linear Sampler RenderSystem

data PassRes px = PassRes
  { vao          :: VertexArray
  , pipe         :: Pipeline
  , frag         :: FragmentShader px
  }

type LinearGaussianIn px = (RenderTarget (Texture2D px), Texture2D px, Maybe (Texture2D px))
type LinearGaussianPass m px = Pass (PassRes px) m (LinearGaussianIn px) (Texture2D px)

linearGaussianSampler :: forall px m. (ImageFormat px, Functor m, MonadIO m, MonadThrow m) => LinearSamplingDirection -> YageResource (LinearGaussianPass m px)
linearGaussianSampler direction = Pass <$> passRes <*> pure runPass where
  passRes :: YageResource (PassRes px)
  passRes = do
    emptyvao <- glResource
    boundVertexArray $= emptyvao

    pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
                , $(embedShaderFile "res/glsl/sampling/gaussian.frag")]
                `compileShaderPipeline` includePaths

    Just (fragment@FragmentShader{..}) <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

    iDirection    $= direction

    return $ PassRes emptyvao pipeline fragment

  -- RenderRenderSystem
  runPass :: (Functor m, MonadIO m, MonadThrow m) => RenderSystem (ReaderT (PassRes px) m) (LinearGaussianIn px) (Texture2D px)
  runPass = mkStaticRenderPass $ \(outTarget, toFilter, mAdd) -> do
    PassRes{..} <- ask
    throwWithStack $ boundFramebuffer RWFramebuffer $= (outTarget^.framebufferObj)

    glDepthMask GL_TRUE
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW

    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao

    -- set shader uniforms

    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    let FragmentShader{..} = frag
    let V2 inWidth inHeight = toFilter^.asRectangle.extend

    iToFilter $= toFilter
    iAdditive $= mAdd
    iUsedTextures $= if isJust mAdd then 2 else 1
    iTargetSize $= V2 inWidth inHeight

    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3

    return $ outTarget^.renderTarget


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
