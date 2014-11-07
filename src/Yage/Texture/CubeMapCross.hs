module Yage.Texture.CubeMapCross where

import               Yage.Prelude
import               Yage.Lens                          ( (<&>) )
import               Yage.Math

import               Yage.Rendering.Textures
import               Yage.Geometry.D2.Rectangle


data CrossOrientation = HorizontalCross | VerticalCross deriving ( Show, Ord, Eq )

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



seperateCubeMapCross :: CrossOrientation -> TextureImage -> TextureCube
seperateCubeMapCross crossOrientation texImg =
    withTextureImageCtr texImg $ \(ctr, img) ->
        let srcWidth    = imageWidth img
            srcHeight   = imageHeight img
            faceWidth   = srcWidth `div` 3
            faceHeight  = srcHeight `div` 4
            ranges      = case crossOrientation of
                            HorizontalCross -> horizontalCross
                            VerticalCross   -> verticalCross

        in ranges <&> (\range -> ctr . GLTexture $
            generateImage (copyRange img range) faceWidth faceHeight)

    where
    copyRange src (Rectangle (V2 x0 y0) (V2 x1 y1)) x y =
        let srcWidth  = fromIntegral $ imageWidth src
            srcHeight = fromIntegral $ imageHeight src
            dirX      = floor . signum $ x1 - x0
            dirY      = floor . signum $ y1 - y0
        in pixelAt src
                (floor (x0 * srcWidth)  + dirX * x)
                (floor (y0 * srcHeight) + dirY * y)
