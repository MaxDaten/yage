{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.Color
    ( module Yage.Color
    , module Color
    ) where

import Yage.Prelude
import Yage.Math (clamp)

import Data.Colour.SRGB                         as Color
import Data.Colour.SRGB.Linear                  as Color
import Data.Colour.CIE                          as Color
import Data.Colour                              as Color
import Data.Colour.Names                        as Color
import JuicySRGB                                as Color
import Codec.Picture
import Codec.Picture.Types

import Linear                                   (V3(..), V4(..))


-- | creates a V3 from a color (for opengl exchange)
-- the value will be in linear color space
linearV3 :: Fractional a => Colour a -> V3 a
linearV3 c = let RGB r g b = toRGB c in V3 r g b


sRGBV3 :: (Ord a, Floating a) => Colour a -> V3 a
sRGBV3 c = let RGB r g b = toSRGB c in V3 r g b


linearV4 :: Fractional a => AlphaColour a -> V4 a
linearV4 ca =
    let a           = alphaChannel ca
        V3 r g b    = linearV3 $ ca `over` black
    in V4 r g b a


sRGBV4 :: (Ord a, Floating a) => AlphaColour a -> V4 a
sRGBV4 ca =
    let a           = alphaChannel ca
        V3 r g b    = sRGBV3 $ ca `over` black
    in V4 r g b a

instance (Num a) => Semigroup (Colour a)
instance (Num a) => Semigroup (AlphaColour a)


instance Default (AlphaColour Double) where
    def = opaque white


instance (Floating a, RealFrac a) => ColourPixel a Pixel8 where
    colourToPixel c = computeLuma (colourToPixel c :: PixelRGB8)
