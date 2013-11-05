{-# LANGUAGE ParallelListComp #-}
module Main where

import Yage.Prelude

import Data.List (sortBy)
import Control.Monad (when)
import Yage.Texture.Atlas
import Codec.Picture
import System.Random



{--
main :: IO ()
main =
    let target = generateImage (\x y -> PixelRGB8 (fromIntegral x) (fromIntegral y) 128) 512 512
        sub    = generateImage (\x y -> PixelRGB8 (fromIntegral y) (fromIntegral x) 128) 128 128
        region = Rectangle 50 50 (50+128) (50+128)
        subbed = subImage sub region target
    in do
        writePng "target.png" target
        writePng "sub.png" sub
        writePng "subbed.png" subbed
--}


main :: IO ()
main = do
    randImgs <- generateRandomImages 60
    let bgrnd          = PixelRGB8 0 0 0
        atlas'         = emptyAtlas 1024 1024 bgrnd 1
        texs           = sortBy imageByAreaCompare randImgs `piz` ([0..] :: [Int])
        (errs, atlas)  = insertImages texs atlas'

    print $ show (errs)
    print $ show (regionMap atlas)
    when (null errs) $ writePng "atlas.png" $ atlasToImage atlas

generateRandomImages :: Int -> IO [Image PixelRGB8]
generateRandomImages count = do
    rs   <- randomlist 0 255
    gs   <- randomlist 0 255
    bs   <- randomlist 0 255
    wp   <- randomlist 5 200 :: IO [Int]
    hp   <- randomlist 5 200 :: IO [Int]
    return $ take count [makeImg (PixelRGB8 r g b) w h | r <- rs | g <- gs | b <- bs | w <- wp | h <- hp]
    where
        makeImg :: (Pixel a) => a -> Int -> Int -> Image a
        makeImg col w h = generateImage (\_ _ -> col) w h

randomlist :: Random a => a -> a -> IO [a]
randomlist a b = fmap (randomRs (a,b)) newStdGen

imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imgRectangle a
        rB = imgRectangle b
    in compare rB rA -- flipped to sort descending
