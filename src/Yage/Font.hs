{-# OPTIONS_GHC -fno-warn-name-shadowing      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Font
    ( module Yage.Font
    , module FT
    , module TB
    , module FTExport
    ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math (V2(..))

import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)
import qualified Graphics.Font as F

import Yage.Font.FontTexture as FT
import Yage.Font.TextBuffer as TB
import Yage.Texture.Atlas.Builder
import Yage.Material as Mat

pt :: Num a => Getter a a
pt = to F.pt


loadFontTexture :: MonadIO m => FilePath -> FontDescriptor -> FontMarkup -> (Int, Int) -> Int -> [Char] -> m FontTexture
loadFontTexture file descr markup (w,h) padding fontchars = do
    lib  <- io $ makeLibrary
    font <- io $ F.loadFont lib ( fpToString file ) descr

    let settings   = AtlasSettings (V2 w h) (0 :: Mat.Pixel8) padding
        tex        = FT.generateFontBitmapTexture font markup F.Monochrome fontchars settings

    either ( \e -> error $ "loadFontTexture: " ++ show e ) ( return ) tex
