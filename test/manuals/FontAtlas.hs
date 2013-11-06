module Main where

import Yage.Texture.Atlas
import Yage.Images

import Graphics.Font
import Control.Monad

import System.FilePath
import Data.List
import Data.Map (toList)
import Codec.Picture


fontPath = "res" </> "font" </> "SourceCodePro-Regular.otf"
main = do
    let descr = FontDescriptor
            { charSize = (0, 16*64)
            , deviceRes = (600, 600)
            }
    font <- loadFont fontPath descr
    let imgs = toList $ generateAllCharImgs font

    let bgrnd          = 0 :: Pixel8
        atlasSize      = 2^11
        atlas'         = emptyAtlas atlasSize atlasSize bgrnd 1
        texs           = sortBy (\(_,img1) (_,img2) -> imageByAreaCompare img1 img2) imgs
        (errs, atlas)  = insertImages texs atlas'

    print $ show (errs)
    print $ show (regionMap atlas)
    writePng "atlas.png" $ atlasToImage atlas

    --forM_ (toList imgs) $ \(c, img) -> do
    --    writePng (charFolder </> "char" ++ (show $ ord c) ++ ".png") img


imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imgRectangle a
        rB = imgRectangle b
    in compare rB rA -- flipped to sort descending