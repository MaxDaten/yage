{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Font
    ( module Yage.Font
    , module FT
    , module TB
    , module FTExport
    ) where

import Yage.Prelude (Num)
import Yage.Lens

import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)
import qualified Graphics.Font as F

import Yage.Font.FontTexture as FT
import Yage.Font.TextBuffer as TB


pt :: Num a => Getter a a
pt = to F.pt
