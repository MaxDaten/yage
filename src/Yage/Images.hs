module Yage.Images
  ( module Yage.Images
  , module Tex
  , module Color
  ) where


import Yage.Prelude
import Yage.Math
import Yage.Lens

import Yage.Color

import Yage.Rendering.Textures                  as Tex
import JuicySRGB                                as Color

import Yage.Geometry.D2.Rectangle

type ImageDimension = V2 Int
type ImagePosition = V2 Int


constImage :: Pixel a => ImageDimension -> a -> Image a
constImage (V2 w h) value = generateImage (const . const $ value) w h


constColorImage :: ( RealFrac a, Floating a, ColourPixel a p ) => ImageDimension -> Colour a -> Image p
constColorImage dim = constImage dim . colourToPixel


constPx :: Pixel a => a -> Image a
constPx = constImage 1


constColorPx :: ( RealFrac a, Floating a, ColourPixel a p ) => Colour a -> Image p
constColorPx = constColorImage 1



subImage :: (Pixel a) => Image a -> ImagePosition -> Image a -> Image a
subImage sub atPx@(V2 atX atY) target
    | not subImageFit = error $ unpack $ format "sub image does not fit at \"{}\" in target image" (Only $ Shown atPx)
    | otherwise   = generateImage includeRegionImg (imageWidth target) (imageHeight target)
    where
        includeRegionImg px py =
          if subRegion `containsPoint` (fromIntegral <$> V2 px py)
            -- pixel is sourced from sub image
            then pixelAt sub (px - subRegion^.width) (py - subRegion^.height)
            -- pixel is sourced from target image
            else pixelAt target px py

        subRegion :: Rectangle Int
        subRegion = imageRectangle sub `translate` atPx
        subImageFit
          = imageWidth target  >= imageWidth sub + atX  &&
            imageHeight target >= imageHeight sub + atY


imageDimension :: Image a -> ImageDimension
imageDimension img = V2 (imageWidth img - 1) (imageHeight img - 1)



imageRectangle :: Image a -> Rectangle Int
imageRectangle img = Rectangle 0 $ imageDimension img


imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imageRectangle a
        rB = imageRectangle b
    in rA `compareArea` rB
