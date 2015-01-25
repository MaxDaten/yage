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


-- redundancy Yage.Rendering.Pipeline.Deferred.GaussianBlur.blurPass will be fixed with 'YageResource' factored out
addBloom :: (ImageFormat px, MonadResource m) => Int -> YageResource (Pass m (Float,Texture px) (Texture px))
addBloom numSamples = do
  sceneHalf         <- lmap (2,) <$> downsampler
  halfSamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  filterLuma <- luminanceFilter
  return $ proc (thrshold, inTex) -> do
    half     <- sceneHalf -< inTex
    filtered <- filterLuma -< (thrshold,half)
    downsampledTextures <- processDownsamples halfSamplers -< [(2::Int,filtered)]
    processGauss gaussianSamplers -< (map snd downsampledTextures, Nothing)
 where
  processGauss :: Monad m => [Pass m (Texture px, Maybe (Texture px)) (Texture px)] -> Pass m ([Texture px], Maybe (Texture px)) (Texture px)
  processGauss [] = rmap (fromJust.snd) id
  processGauss (s:ss) = proc ((t:texs), madd) -> do
    out <- s -< (t, madd)
    processGauss ss -< (texs, Just out)

  processDownsamples :: Monad m => [Pass m (Texture px) (Texture px)] -> Pass m [(Int,Texture px)] [(Int,Texture px)]
  processDownsamples [] = id
  processDownsamples (s:ss) = proc (t:texs) -> do
    tex <- s -< snd t
    processDownsamples ss -< (2 * fst t,tex):t:texs
