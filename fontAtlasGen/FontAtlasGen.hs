{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
module Main where

import Yage.Prelude hiding (sortBy, toList)

import Yage.Texture.Atlas
import Yage.Images

import Graphics.Font
import Control.Monad

import Codec.Picture
import Data.List (sortBy)
import Data.Map (toList)

fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Regular.otf"
atlasFile = encodeString $ "atlas" <.> "png"

main = withNewLibrary $ \lib -> do
    let !descr = traceShowId $! FontDescriptor
            { charSize = (0, 16*64)
            , deviceRes = (600, 600)
            }

    printTF "start loading font-file: {}\n" (Only fontPath)
    font <- loadFont lib fontPath descr

    
    printTF "generate char bitmaps for {}\n" (Only $ Shown $ fontName font)
    imgs <- toList <$> generateAllCharImgs font Monochrome
    printTF "loaded chars: {}\n" (Only $ Shown $ length imgs)

    let bgrnd          = 0 :: Pixel8
        atlasSize      = 2^11
        atlas'         = emptyAtlas atlasSize atlasSize bgrnd 5
        texs           = sortBy (descending imageByAreaCompare `on` snd) imgs
        (errs, atlas)  = insertImages texs atlas'

    --print $ show (errs)
    print $ show (regionMap atlas)
    printTF "write atlas img {}\n" (Only $ atlasFile)
    printTF "write atlas img {}\n" (Only $ map fst imgs)
    writePng atlasFile $ atlasToImage atlas


