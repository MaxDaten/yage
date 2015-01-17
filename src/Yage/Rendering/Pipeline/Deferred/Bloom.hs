{-# LANGUAGE TupleSections          #-}
module Yage.Rendering.Pipeline.Deferred.Bloom
  ( addBloom
  ) where


import           Yage.Prelude hiding ((</>), foldM, cons, (++))
import           Yage.Lens hiding (cons)
import           Yage.Vertex hiding (Texture)
import           Yage.Formats.Ygm


import           Yage.HDR
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem                     as RenderSystem
import           Yage.Rendering.Resources.GL
import           Yage.Scene
import           Yage.Viewport
import           Yage.Material

import           Yage.Rendering.Pipeline.Deferred.Common         as Pass
import           Yage.Rendering.Pipeline.Deferred.Downsampling   as Pass
import           Yage.Rendering.Pipeline.Deferred.GaussianBlur   as Pass
import           Yage.Rendering.Pipeline.Deferred.LuminanceFilter   as Pass

import           System.FilePath ((</>))
import           Control.Monad (foldM)
import           Data.List ((++))
import           Data.Maybe (fromJust)
import           Quine.GL.Types


addBloom :: ImageFormat px => Int -> YageResource (RenderSystem (Float,Texture px) (Texture px))
addBloom numSamples = do
  sceneHalf  <- lmap (2,) <$> downsampler
  halfSamplers      <- replicateM numSamples $ lmap (2,) <$> downsampler
  gaussianSamplers  <- replicateM (numSamples + 1) $ gaussianSampler
  filterLuma <- luminanceFilter
  return $ do
    (thrshold, inTex) <- ask
    filtered <- filterLuma . fmap (thrshold,) sceneHalf . pure inTex
    downsampledTextures <- reverse <$> foldM processDownsample [(2::Int,filtered)] halfSamplers
    fromJust <$> foldM (\a (gaussian,(_,d)) -> Just <$> gaussian . pure (d,a)) Nothing (zip gaussianSamplers downsampledTextures)
 where
  processDownsample txs dsampler =
    let (lastfactor, base) = unsafeLast txs
    in fmap ((++) txs . singleton . (2*lastfactor,)) dsampler . pure base
