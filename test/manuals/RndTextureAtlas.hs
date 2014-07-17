{-# LANGUAGE ParallelListComp #-}
module Main where

import qualified Prelude

import Yage.Prelude
import Yage.Data.List (piz)
import Yage.Images

import Data.Text.Read
import Control.Monad (when)
import Yage.Texture.Atlas
import Codec.Picture
import System.Random
import Control.Monad.Random



main :: IO ()
main = do
    printUsage
    setSeed
    
    randImgs <- generateRandomImages =<< getImageCount
    let bgrnd          = PixelRGB8 0 0 0
        atlas'         = emptyAtlas 1024 1024 bgrnd 1
        texs           = sortBy (descending imageByAreaCompare) randImgs `piz` ([0..] :: [Int])
        (errs, atlas)  = insertImages texs atlas'

    print $ "images:\n" ++ show (map (imageRectangle . snd) texs)
    print $ "errors:\n" ++ show errs
    print $ "atlas: \n" ++ show (regionMap atlas)
    writePng "atlas.png" $ atlasToImage atlas


generateRandomImages :: Int -> IO [Image PixelRGB8]
generateRandomImages count = replicateM count generateRandomImg
    where
    generateRandomImg = do
        -- because our background color is black we set our lower color bound a bit up
        color <- PixelRGB8 <$> getRandomR (60, 254) 
                           <*> getRandomR (60, 254) 
                           <*> getRandomR (60, 254) 
        makeImg color
            <$> getRandomR (5, 200)
            <*> getRandomR (5, 200)

    makeImg :: (Pixel a) => a -> Int -> Int -> Image a
    makeImg col = generateImage (const . const $ col)


printUsage :: IO ()
printUsage = do
    print "Usage:"
    print "args in this order: seed image-count"


setSeed :: IO ()
setSeed = do
    eSeed <- decimal . (Prelude.!! 0) <$> getArgs
    case eSeed of
        Left err        -> error err
        Right (seed, _) -> do
            setStdGen $ mkStdGen seed
            print $ "seed: " ++ show seed

getImageCount :: IO Int
getImageCount = do
    eCnt <- decimal . (Prelude.!! 1) <$> getArgs
    either error (return . fst) eCnt
