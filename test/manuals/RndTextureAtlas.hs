{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Prelude

import Yage.Prelude
import Yage.Math

import Yage.Data.List (piz)
import Yage.Images

import Data.Text.Read
import Control.Monad (when)
import Codec.Picture
import System.Random
import Control.Monad.Random

import Yage.Texture.Atlas
import Yage.Texture.Atlas.Builder


main :: IO ()
main = do
    printUsage
    setSeed
    
    randImgs <- generateRandomImages =<< getImageCount
    let bgrnd          = PixelRGB8 0 0 0
        settings       = AtlasSettings (V2 1024 1024) bgrnd 1
        texs           = [(0::Int)..] `zip` randImgs

    writeNewAtlasPNG settings "atlas.png" texs


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
    print "e.g. textureAtlas 12345 73"


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
