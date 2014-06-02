{-# LANGUAGE OverloadedStrings #-}
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
main = do
    let descr = FontDescriptor
            { charSize = (0, 16*64)
            , deviceRes = (600, 600)
            }
    font <- loadFont fontPath descr
    let imgs = toList $ generateAllCharImgs font Monochrome

    let bgrnd          = 0 :: Pixel8
        atlasSize      = 2^11
        atlas'         = emptyAtlas atlasSize atlasSize bgrnd 5
        texs           = sortBy (\(_,img1) (_,img2) -> descending imageByAreaCompare img1 img2) imgs
        (errs, atlas)  = insertImages texs atlas'

    print $ show (errs)
    -- print $ show (regionMap atlas)
    writePng atlasFile $ atlasToImage atlas


