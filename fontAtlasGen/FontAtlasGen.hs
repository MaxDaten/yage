{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Yage.Prelude hiding ( sortBy, toList, (<>) )
import Yage.Lens hiding ( (<.>), argument )
import Yage.Math (V2(..))
import Yage.Images
import Yage.Font

import Graphics.Font hiding ( pt )
import Codec.Picture

import Control.Monad
import Control.Monad.State
import Data.List (sortBy)
import Data.Map (toList)

import Yage.Texture.Atlas
import Yage.Texture.Atlas.Builder
import Yage.Image.SDF

import Options.Applicative hiding ( (&) )
import System.ProgressBar

-------------------------------------------------------------------------------

data FontAtlasGen = FontAtlasGen
    { outFile   :: FilePath
    , fontFile  :: FilePath
    , highRes   :: Int
    , lowRes    :: Int
    , devRes    :: Int
    , fontPt    :: Int
    , padding   :: Int
    , sdf       :: Maybe Int
    } deriving ( Show )



main = do
    FontAtlasGen{..} <- execParser $
        info ( helper <*> fontAtlasGenOpts ) $
            fullDesc <> progDesc "Generates a Font Atlas Map"

    withNewLibrary $ \lib -> do

        let descr = FontDescriptor
                { charSize = (fontPt^.pt, fontPt^.pt)
                , deviceRes = (devRes, devRes)
                }
            bgrnd          = 0 :: Pixel8
            settings       = AtlasSettings ( pure highRes ) bgrnd 10


        printTF "start loading font-file: {}\n" (Only $ Shown fontFile)
        font <- loadFont lib (fpToString fontFile) descr


        printTF "generate char bitmaps for {}\n" (Only $ Shown $ fontName font)
        imgs <- toList <$> generateAllCharImgs font Monochrome

        printTF "loaded chars: {}\n" (Only $ Shown $ length imgs)

        case newTextureAtlas settings imgs of
            Left errs   -> printTF "errors: {}\n" (Only $ Shown errs)
            Right atlas -> do
                let ft          = makeFontTexture font (FontMarkup 1.0 1.0) atlas
                    V2 w h      = fromIntegral <$> ft^.fontMap.to imageDimension
                    --sdfProg     :: Image Pixel8 -> IO (Image Pixel8)


                fontTexture <- case sdf of
                    Nothing     -> return ft
                    Just spread -> flip execStateT ft $ do
                        (progRef,_) <- io $ startProgress (msg "generate sdf: ") percentage 100 ((w + 1) * (h + 1))
                        let incr   = incProgress progRef . fromIntegral
                        fontMap    <~ io (signedDistanceFieldProgressed (ft^.fontMap) spread incr)


                printTF "\ndownscale from {} to {}" (Shown (highRes, highRes), Shown (lowRes, lowRes))
                let scaled      = downscaleFontTexture (highRes `div` lowRes) fontTexture

                printTF "\nwrite atlas img to {}" (Only $ Shown outFile)
                writePng (fpToString outFile) (scaled^.fontMap)


fontAtlasGenOpts :: Parser FontAtlasGen
fontAtlasGenOpts =
    FontAtlasGen <$> fileOption ( long "outFile"   <> short 'o' <> metavar "OUTFILE" <> action "file")
                 <*> fileOption ( long "fontFile"  <> short 'f' <> metavar "FONTFILE" <> action "file")
                 <*> option ( long "high"      <> short 'i' <> value 4096 <> metavar "PIXEL" )
                 <*> option ( long "low"       <> short 'l' <> value 512  <> metavar "PIXEL" )
                 <*> option ( long "dev"       <> short 'd' <> value 100  <> metavar "PIXEL" )
                 <*> option ( short 'p'                     <> value 14   <> metavar "PT" )
                 <*> option ( short 'a'                     <> value 10   <> metavar "PIXEL" )
                 <*> ( (Just <$> sdfOption) <|> pure Nothing )
    where
    sdfOption = option ( long "sdf" <> metavar "PIXEL" <> help "Generate a signed distance field before downscaling to LOWRES" )
    fileOption = fmap (fpFromText . pack) . strOption
