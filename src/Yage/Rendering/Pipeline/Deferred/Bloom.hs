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
  dsampler          <- downsampler
  let halfSamplers  = batchedDownsampler dsampler
  gaussPass         <- dimap (\((a,b),c)->(a,b,c)) Just <$> gaussianSampler
  filterLuma <- luminanceFilter
  return $ proc (thrshold, inTexture) -> do

    -- filter luma on half texture
    halfTarget <- autoResized mkTarget -< inTexture^.asRectangle & extend.mapped %~ (`div` 2)
    half       <- processPass dsampler -< (halfTarget,inTexture)
    filteredTex <- filterLuma -< (thrshold,half)

    downTargets         <- mapA (autoResized mkTarget)  -< targetRects (numSamples) (inTexture^.asRectangle)
    downsampledTextures <- halfSamplers                 -< (downTargets,[filteredTex])

    targets             <- mapA (autoResized mkTarget)     -< downsampledTextures & mapped %~ view asRectangle
    let gaussFoldingInput = (zip targets downsampledTextures, Nothing)
    fromJust <$> foldA gaussPass -< gaussFoldingInput
 where
  mkTarget rect = let V2 w h = rect^.extend in createTexture2D GL_TEXTURE_2D (Tex2D w h) 1
  targetRects :: Int -> Rectangle Int -> [Rectangle Int]
  targetRects n src = map ( \i -> src & extend.mapped %~ (\x -> max 1 (x `div` (2^i))) ) $ [1..n]

