module Yage.Texture.CubeImageLayout where

import           Quine.Cubemap
import           Yage.Lens
import           Yage.Math
import           Yage.Image
import           Yage.Prelude

import           Yage.Geometry.D2.Rectangle


data CubeImageLayout = HorizontalCross | VerticalCross | Strip deriving ( Show, Read, Ord, Eq, Typeable )

type Layout = (V2 Int, Cubemap (Rectangle Int))
-- | column & row ranges for a standard vertical cube map cross
verticalCross :: Layout
verticalCross = (V2 3 4, Cubemap
    { faceRight     = Rectangle (V2 2 1) (V2 3 2)
    , faceLeft      = Rectangle (V2 0 1) (V2 1 2)
    , faceTop       = Rectangle (V2 1 0) (V2 2 1)
    , faceBottom    = Rectangle (V2 1 2) (V2 2 3)
    , faceFront     = Rectangle (V2 1 1) (V2 2 2)
    , faceBack      = Rectangle (V2 2 4) (V2 1 3)
    -- ^ flipped back
    })


-- | column & row ranges for a standard vertical cube map cross
horizontalCross :: Layout
horizontalCross = (V2 4 3, Cubemap
    { faceRight     = Rectangle (V2 2 1) (V2 3 2)
    , faceLeft      = Rectangle (V2 0 1) (V2 1 2)
    , faceTop       = Rectangle (V2 1 0) (V2 2 1)
    , faceBottom    = Rectangle (V2 1 2) (V2 2 3)
    , faceFront     = Rectangle (V2 1 1) (V2 2 2)
    , faceBack      = Rectangle (V2 3 1) (V2 4 2)
    })

-- | column & row ranges for a cube map strip
stripLayout :: Layout
stripLayout = (V2 6 1, Cubemap
    { faceRight     = Rectangle (V2 0 0) (V2 1 1)
    , faceLeft      = Rectangle (V2 1 0) (V2 2 1)
    , faceTop       = Rectangle (V2 2 0) (V2 3 1)
    , faceBottom    = Rectangle (V2 3 0) (V2 4 1)
    , faceFront     = Rectangle (V2 4 0) (V2 5 1)
    , faceBack      = Rectangle (V2 5 0) (V2 6 1)
    })

imageRegions :: CubeImageLayout -> Layout
imageRegions HorizontalCross = horizontalCross
imageRegions VerticalCross = verticalCross
imageRegions Strip = stripLayout

seperateCubeMapImage :: Pixel a => CubeImageLayout -> Image a -> Cubemap (Image a)
seperateCubeMapImage layout img =
    let srcWidth            = imageWidth img
        srcHeight           = imageHeight img
        faceWidth           = srcWidth `div` xR
        faceHeight          = srcHeight `div` yR
        (V2 xR yR, ranges)  = imageRegions layout & _2.mapped %~ (`rescale` (V2 faceWidth faceHeight))
    in ranges <&> (\range -> generateImage (copyRange img range) faceWidth faceHeight)

    where
    copyRange src (Rectangle (V2 x0 y0) (V2 x1 y1)) x y =
        let dirX      = signum $ x1 - x0
            dirY      = signum $ y1 - y0
        in pixelAt src (x0 + dirX * x) (y0 + dirY * y)
