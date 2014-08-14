{-# LANGUAGE FlexibleContexts #-}
module Yage.Image.SDF
    ( signedNearDistance
    , signedDistanceFieldProgressed
    , signedDistanceField
    ) where

import Yage.Prelude
import Yage.Math

import Control.Monad.ST
import Codec.Picture
import Codec.Picture.Types

data InOut = Inside | Outside
    deriving (Show, Eq)


signedDistanceFieldProgressed :: Image Pixel8 -> Int -> (Int -> IO ()) -> IO (Image Pixel8)
signedDistanceFieldProgressed bitmap spread incrCall = do
    let w   = imageWidth bitmap
        h   = imageHeight bitmap

    img <- createMutableImage w h (minBound :: Pixel8)
    forM_ [0..h-1] $ \y -> do
        forM_ [0..w-1] $ \x ->
            writePixel img x y $ signedNearDistance bitmap spread x y
        incrCall w

    unsafeFreezeImage img

{--
    where

    parImage :: (Storable (PixelBaseComponent a), NFData (PixelBaseComponent a)) => Strategy (Image a)
    parImage Image{..} = return $ Image imageWidth imageHeight (imageData `using` parVector imageWidth)
--}


signedDistanceField :: Image Pixel8 -> Int -> Image Pixel8
signedDistanceField bitmap spread = runST $ do
    let w   = imageWidth bitmap
        h   = imageHeight bitmap

    img <- createMutableImage w h (minBound :: Pixel8)
    forM_ [0..h-1] $ \y ->
        forM_ [0..w-1] $ \x ->
            writePixel img x y (signedNearDistance bitmap spread x y)

    unsafeFreezeImage img


signedNearDistance :: Image Pixel8 -> Int -> Int -> Int -> Pixel8
signedNearDistance bitmap spread x y =
    let s, upperPx, maxDist2 :: Double
        s         = fromIntegral spread
        maxDist2  = s * s
        upperPx   = fromIntegral (maxBound :: Pixel8)
        halfPx    = upperPx / 2.0

        msqr      = headMay $ sort distSqr's
        signing   = if activeSide == Inside then id else negate
        dist      = signing $ sqrt $ maybe maxDist2 fromIntegral msqr
        --val       = round $ dist * ( half / s ) + half
        --val       = round $ (dist + s) / ( 2.0 * s ) * upperPx
        val       = round $ dist / s * halfPx + halfPx
    in clamp val minBound maxBound

    where

    activeSide      = toInside $ pixelAt bitmap x y
    {-# INLINE activeSide #-}

    toInside px     = if px == maxBound then Inside else Outside
    {-# INLINE toInside #-}

    distSqr's       = do
        offx <- [ -spread .. spread ]
        offy <- [ -spread .. spread ]
        guard $ (offx, offy) /= (0,0)

        -- out of radius
        let distSqr = offx * offx + offy * offy
        guard $ distSqr <= spread * spread

        let xi = x + offx
            yi = y + offy
        guard $ xi >= 0 && xi < imageWidth bitmap
        guard $ yi >= 0 && yi < imageHeight bitmap

        -- pixel is on the other side?
        guard $ toInside (pixelAt bitmap xi yi) /= activeSide
        return distSqr
{-# INLINE signedNearDistance #-}
