{-# LANGUAGE ParallelListComp #-}
module Main where

import Yage.Prelude
import Yage.Images

import Data.List (sortBy)
import Control.Monad (when)
import Yage.Texture.Atlas
import Codec.Picture
import System.Random



main :: IO ()
main = do
    randImgs <- generateRandomImages 60
    let bgrnd          = PixelRGB8 0 0 0
        atlas'         = emptyAtlas 1024 1024 bgrnd 1
        texs           = sortBy (descending imageByAreaCompare) randImgs `piz` ([0..] :: [Int])
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
