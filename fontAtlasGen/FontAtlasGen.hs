{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Yage.Prelude hiding (sortBy, toList)
import Yage.Lens hiding ((<.>))

import Yage.Images

import Graphics.Font
import Codec.Picture

import Control.Monad
import Data.List (sortBy)
import Data.Map (toList)

import Yage.Texture.Atlas
import Yage.Texture.Atlas.Builder

-------------------------------------------------------------------------------

--fontPath  = encodeString $ "res" </> "font" </> "SourceCodePro-Regular.otf"
--atlasFile = "atlas" <.> "png"

main = withNewLibrary $ \lib -> do
    printUsage
    (fontPath::FilePath, atlasFile::FilePath) <- readArgs
    
    let descr = FontDescriptor
            { charSize = (0, 20^.to pt)
            , deviceRes = (300, 300)
            }
    

    printTF "start loading font-file: {}\n" (Only $ Shown fontPath)
    font <- loadFont lib (fpToString fontPath) descr

    
    printTF "generate char bitmaps for {}\n" (Only $ Shown $ fontName font)
    imgs <- toList <$> generateAllCharImgs font Monochrome
    
    printTF "loaded chars: {}\n" (Only $ Shown $ length imgs)

    let bgrnd          = 0 :: Pixel8
        settings       = AtlasSettings (2^11) bgrnd 10

    printTF "write atlas img to {}\n" (Only $ Shown atlasFile)
    writeNewAtlasPNG settings atlasFile imgs

printUsage :: IO ()
printUsage = do
    print "Usage:"
    print "e.g: fontAtlasGen res/font/SourceCodePro-Regular.otf atlasfont.png"
