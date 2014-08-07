module Yage.Image.SDF
    ( signedNearDistance
    , signedDistanceField
    ) where

import Yage.Prelude

import Control.Monad.ST
import Codec.Picture
import Codec.Picture.Types

data InOut = Inside | Outside
    deriving (Show, Eq)


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
    let s, upperPx, maxDist :: Double
        s         = fromIntegral spread
        maxDist   = sqrt $ 2 * s * s
        upperPx   = fromIntegral (maxBound :: Pixel8)

        msqr      = headMay $ sort distSqr's
        signing   = if activeSide == Inside then id else negate
        dist      = maybe (signing maxDist) (signing . sqrt . fromIntegral) msqr
        val       = round $ (dist + maxDist) / (2.0 * maxDist) * upperPx
    in clamp maxBound minBound val

    where
    clamp upper lower = min upper . max lower


    activeSide      = toInside $ pixelAt bitmap x y
    toInside px     = if px == maxBound then Inside else Outside
    -- isInside x y    = toInside (pixelAt bitmap x y) == Inside

    distSqr's = [ dx*dx + dy*dy | (dx,dy) <- others ]

    others = do
        offx <- [ -spread .. spread ]
        offy <- [ -spread .. spread ]
        let xi = x + offx
            yi = y + offy
        guard $ (offx, offy) /= (0,0)
        guard $ xi >= 0 && xi < imageWidth bitmap
        guard $ yi >= 0 && yi < imageHeight bitmap
        -- is on the other side?
        guard $ toInside (pixelAt bitmap xi yi) /= activeSide
        return (offx, offy)
