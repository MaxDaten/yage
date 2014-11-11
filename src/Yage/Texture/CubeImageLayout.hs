module Yage.Texture.CubeImageLayout where

import               Yage.Prelude
import               Yage.Lens                          ( (<&>) )
import               Yage.Math

import               Yage.Rendering.Textures
import               Yage.Geometry.D2.Rectangle


data CubeImageLayout = HorizontalCross | VerticalCross | Strip deriving ( Show, Read, Ord, Eq, Typeable )

-- | normalized ranges for a standard vertical cube map cross
verticalCross :: Cube (Rectangle Double)
verticalCross =
    let x3rd = 1/3
        y4rd = 1/4
    in Cube
    { cubeFaceRight     = Rectangle (V2 (2 * x3rd) (1 * y4rd)) (V2 (3 * x3rd) (2 * y4rd))
    , cubeFaceLeft      = Rectangle (V2 (0 * x3rd) (1 * y4rd)) (V2 (1 * x3rd) (2 * y4rd))
    , cubeFaceTop       = Rectangle (V2 (1 * x3rd) (0 * y4rd)) (V2 (2 * x3rd) (1 * y4rd))
    , cubeFaceBottom    = Rectangle (V2 (1 * x3rd) (2 * y4rd)) (V2 (2 * x3rd) (3 * y4rd))
    , cubeFaceFront     = Rectangle (V2 (1 * x3rd) (1 * y4rd)) (V2 (2 * x3rd) (2 * y4rd))
    , cubeFaceBack      = Rectangle (V2 (2 * x3rd) (4 * y4rd)) (V2 (1 * x3rd) (3 * y4rd))
    -- ^ flipped back
    }


-- | normalized ranges for a standard vertical cube map cross
horizontalCross :: Cube (Rectangle Double)
horizontalCross =
    let x4rd = 1/4
        y3rd = 1/3
    in Cube
    { cubeFaceRight     = Rectangle (V2 (2 * x4rd) (1 * y3rd)) (V2 (3 * x4rd) (2 * y3rd))
    , cubeFaceLeft      = Rectangle (V2 (0 * x4rd) (1 * y3rd)) (V2 (1 * x4rd) (2 * y3rd))
    , cubeFaceTop       = Rectangle (V2 (1 * x4rd) (0 * y3rd)) (V2 (2 * x4rd) (1 * y3rd))
    , cubeFaceBottom    = Rectangle (V2 (1 * x4rd) (2 * y3rd)) (V2 (2 * x4rd) (3 * y3rd))
    , cubeFaceFront     = Rectangle (V2 (1 * x4rd) (1 * y3rd)) (V2 (2 * x4rd) (2 * y3rd))
    , cubeFaceBack      = Rectangle (V2 (3 * x4rd) (1 * y3rd)) (V2 (4 * x4rd) (2 * y3rd))
    }

-- | normalized ranges for a cube map strip
stripCross :: Cube (Rectangle Double)
stripCross =
    let x6rd = 1/6
    in Cube
    { cubeFaceRight     = Rectangle (V2 (0 * x6rd) 0) (V2 (1 * x6rd) 1)
    , cubeFaceLeft      = Rectangle (V2 (1 * x6rd) 0) (V2 (2 * x6rd) 1)
    , cubeFaceTop       = Rectangle (V2 (2 * x6rd) 0) (V2 (3 * x6rd) 1)
    , cubeFaceBottom    = Rectangle (V2 (3 * x6rd) 0) (V2 (4 * x6rd) 1)
    , cubeFaceFront     = Rectangle (V2 (4 * x6rd) 0) (V2 (5 * x6rd) 1)
    , cubeFaceBack      = Rectangle (V2 (5 * x6rd) 0) (V2 (6 * x6rd) 1)
    }

imageRegions :: CubeImageLayout -> Cube (Rectangle Double)
imageRegions HorizontalCross = horizontalCross
imageRegions VerticalCross = verticalCross
imageRegions Strip = stripCross

seperateCubeMapImage :: CubeImageLayout -> TextureImage -> TextureCube
seperateCubeMapImage crossOrientation texImg =
    withTextureImageCtr texImg $ \(ctr, img) ->
        let srcWidth    = imageWidth img
            srcHeight   = imageHeight img
            faceWidth   = srcWidth `div` 3
            faceHeight  = srcHeight `div` 4
            ranges      = imageRegions crossOrientation
        in ranges <&> (\range -> ctr . GLTexture $
            generateImage (copyRange img range) faceWidth faceHeight)

    where
    copyRange src (Rectangle (V2 x0 y0) (V2 x1 y1)) x y =
        let srcWidth  = fromIntegral $ imageWidth src - 1
            srcHeight = fromIntegral $ imageHeight src - 1
            dirX      = floor . signum $ x1 - x0
            dirY      = floor . signum $ y1 - y0
        in pixelAt src
                (round (x0 * srcWidth)  + dirX * x)
                (round (y0 * srcHeight) + dirY * y)
