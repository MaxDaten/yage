{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Yage.Image.BilinearInterpolation where

import Yage.Prelude
import Yage.Math
import Control.Monad.ST

import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


bilinear :: forall p. (Pixel p, Integral (PixelBaseComponent p)) => Int -> Int -> Image p -> Image p
bilinear newWidth newHeight img =
    let width    = imageWidth img
        height   = imageHeight img
        tx, ty :: Double
        tx       = fromIntegral (width - 1) / fromIntegral newWidth
        ty       = fromIntegral (height - 1) / fromIntegral newHeight
        proxy    = pixelAt img 0 0
        chs      = traceShowId $ componentCount proxy
    in runST $ do
        mImg <- createMutableImage newWidth newHeight proxy

        forM_ [0..newHeight-1] $ \yi ->
            forM_ [0..newWidth-1] $ \xj ->
                let x  = floor $ tx * fromIntegral xj
                    y  = floor $ ty * fromIntegral yi
                    dx = tx * fromIntegral xj - fromIntegral x
                    dy = ty * fromIntegral yi - fromIntegral y
                    -- nodes
                in forM_ [0..chs-1] $ \ch ->
                    let a = pixelAtClamp img x     y     ch
                        b = pixelAtClamp img (x+1) y     ch
                        c = pixelAtClamp img x     (y+1) ch
                        d = pixelAtClamp img (x+1) (y+1) ch

                        index = traceShowId $ ch + mutablePixelBaseIndex mImg xj yi
                        interpolate = bilin proxy
                    in M.unsafeWrite (mutableImageData mImg) index $ interpolate dx dy a b c d

        unsafeFreezeImage mImg

    where

    bilin :: (Pixel p, Integral (PixelBaseComponent p)) => p -> Double -> Double -> PixelBaseComponent p -> PixelBaseComponent p -> PixelBaseComponent p -> PixelBaseComponent p -> PixelBaseComponent p
    bilin _proxy dx dy a b c d =
        let a_ = realToFrac a
            b_ = realToFrac b
            c_ = realToFrac c
            d_ = realToFrac d
        in round $
           a_ * ( 1 - dx ) * ( 1 - dy )
         + b_ * ( 1 - dy ) * dx
         + c_ * dy         * ( 1 - dx )
         + d_ * dy         * dx
    {-# INLINE bilin #-}


pixelAtClamp :: Pixel p => Image p -> Int -> Int -> Int -> (PixelBaseComponent p)
pixelAtClamp img x y c =
    let w     = imageWidth img - 1
        h     = imageHeight img - 1
        index = c + pixelBaseIndex img (clamp 0 w x) (clamp 0 h y)
    in V.unsafeIndex (imageData img) index
{-# INLINE pixelAtClamp #-}

