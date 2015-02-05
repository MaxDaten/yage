{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE Arrows                 #-}
module Yage.Rendering.Pipeline.Deferred.Bloom
  ( addBloom
  ) where


import           Yage.Prelude hiding ((</>), foldM, cons, (++))
import           Yage.Lens
import           Yage.Math (V2(V2))

import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.RenderTarget
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
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
  gaussPass         <- dimap (\((a,b),c)->(a,b,c)) Just <$> gaussianSampler
  filterLuma <- luminanceFilter
  return $ proc (thrshold, inTex) -> do
    half     <- sceneHalf -< inTex
    filteredTex <- filterLuma -< (thrshold,half)
    downsampledTextures <- processDownsamples halfSamplers -< [(2::Int,filteredTex)]
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
