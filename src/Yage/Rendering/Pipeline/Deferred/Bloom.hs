{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE Arrows                 #-}
module Yage.Rendering.Pipeline.Deferred.Bloom
  ( addBloom
  ) where


import           Yage.Prelude hiding ((</>), foldM, cons, (++))
import           Yage.Lens

import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.Resources.GL
import           Yage.Scene

import           Yage.Rendering.Pipeline.Deferred.Downsampling   as Pass
import           Yage.Rendering.Pipeline.Deferred.GaussianBlur   as Pass
import           Yage.Rendering.Pipeline.Deferred.LuminanceFilter   as Pass

import           Data.Maybe (fromJust)


-- redundancy Yage.Rendering.Pipeline.Deferred.GaussianBlur.blurRenderSystem will be fixed with 'YageResource' factored out
addBloom :: (ImageFormat px, MonadResource m) => Int -> YageResource (RenderSystem m (Float,Texture2D px) (Texture2D px))
addBloom numSamples = do
  sceneHalf         <- lmap (2,) <$> downsampler
  halfSamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  filterLuma <- luminanceFilter
  return $ proc (thrshold, inTex) -> do
    half     <- sceneHalf -< inTex
    filteredTex <- filterLuma -< (thrshold,half)
    downsampledTextures <- processDownsamples halfSamplers -< [(2::Int,filteredTex)]
    processGauss gaussianSamplers -< (map snd downsampledTextures, Nothing)
 where
  processGauss :: Monad m => [RenderSystem m (Texture2D px, Maybe (Texture2D px)) (Texture2D px)] -> RenderSystem m ([Texture2D px], Maybe (Texture2D px)) (Texture2D px)
  processGauss [] = rmap (fromJust.snd) id
  processGauss (s:ss) = proc ((t:texs), madd) -> do
    out <- s -< (t, madd)
    processGauss ss -< (texs, Just out)

  processDownsamples :: Monad m => [RenderSystem m (Texture2D px) (Texture2D px)] -> RenderSystem m [(Int,Texture2D px)] [(Int,Texture2D px)]
  processDownsamples [] = id
  processDownsamples (s:ss) = proc (t:texs) -> do
    tex <- s -< snd t
    processDownsamples ss -< (2 * fst t,tex):t:texs
