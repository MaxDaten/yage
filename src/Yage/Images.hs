module Yage.Images
  ( module Yage.Images
  , module JT
  ) where


import Yage.Prelude
import Yage.Math
import Yage.Lens


import Codec.Picture                            as JT
import Codec.Picture.Types                      as JT

import Yage.Geometry.D2.Rectangle


subImage :: (Pixel a) => Image a -> V2 Int -> Image a -> Image a
subImage sub atPx@(V2 atX atY) target 
    | not subImageFit = error $ format "sub image does not fit at \"{0}\" in target image" [show atPx]
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


imageDimension :: Image a -> V2 Int
imageDimension img = V2 (imageWidth img - 1) (imageHeight img - 1)



imageRectangle :: Image a -> Rectangle Int
imageRectangle img = Rectangle 0 $ imageDimension img 


imageByAreaCompare :: Image a -> Image a -> Ordering
imageByAreaCompare a b =
    let rA = imageRectangle a
        rB = imageRectangle b
    in rA `compareArea` rB