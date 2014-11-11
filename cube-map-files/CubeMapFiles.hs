{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Foldable              (sequenceA_, toList)
import           Data.List                  ((!!))
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.List.NonEmpty         as N
import           Data.Traversable
import           Filesystem.Path            (dropExtensions)
import           Yage.Lens
import           Yage.Math
import           Yage.Prelude               hiding (toList)


import           GHC.Exts                   (groupWith)

import           Codec.Picture
import           Text.Read
import           Text.Regex.Posix

import           Yage.Formats.AMDCubeMap
import           Yage.Rendering.Textures
import           Yage.Texture
import           Yage.Texture.CubeImageLayout

import           Yage.Geometry.D2.Rectangle

import qualified Yage.Core.OpenGL           as GL




-- | merge seperate cube map files
main :: IO ()
main = do
    ( inDirGlob  :: FilePath
        , outMode   :: CubeImageLayout
        , outFile   :: FilePath ) <- readArgs

    mips <- groupWith (fst.matchLevelAndSide) <$> globFp inDirGlob
    let cubes     = map (cubeFromList.sortWith (snd.matchLevelAndSide)) $ mips
        mMipCubes :: Maybe (MipMapChain (Cube FilePath))
        mMipCubes = mipMapChain cubes

    case mMipCubes of
        Nothing -> io $ throwIO $ LoadCubeMapException $
                        "at least one complete cube map with base texture for MipMapChain required!"
        Just mipCubes  -> do
            cubeMipsImgs <- (traverse.traverse) loadImage mipCubes
            let outMips     :: MipMapChain (Cube TextureImage)
                outMips     = cubeMipsImgs
                mipFilename :: Int -> FilePath
                mipFilename i = fpFromText . toStrict $ format "{}_m{}" (dropExtensions outFile, left 2 '0' i)
            sequenceA_ $ N.zipWith ( \c idx -> writeOut outMode (mipFilename idx) c) outMips (0 :| [1..])

    where
    -- | the center of the conversion
    writeOut :: CubeImageLayout -> FilePath -> Cube TextureImage -> IO ()
    writeOut orient fp cube@Cube{cubeFaceRight} =
        let out = case cubeFaceRight of
                    TexRGB8  (GLTexture img) ->
                        TexRGB8  $ GLTexture $ mergeCube orient (PixelRGB8 0 0 0) $ fmap toRGB8 cube
                    TexRGBA8 (GLTexture img) ->
                        TexRGBA8 $ GLTexture $ mergeCube orient (PixelRGBA8 0 0 0 0) $ fmap toRGBA8 cube
                    TexRGBF (GLTexture img)  ->
                        TexRGBF  $ GLTexture $ mergeCube orient (PixelRGBF 0 0 0) $ fmap toRGBF cube
                    tex -> error $ unpack $ format "unsported format: {}" (Only $ Shown $ debugString tex)
        in writeTextureImage fp out

    toRGB8 :: TextureImage -> (Image PixelRGB8)
    toRGB8 (TexRGB8 glimg) = unGLTexture glimg
    toRGB8 _ = error "image conversion not possible"

    toRGBA8 :: TextureImage -> (Image PixelRGBA8)
    toRGBA8 (TexRGBA8 glimg) = unGLTexture glimg
    toRGBA8 _ = error "image conversion not possible"

    toRGBF :: TextureImage -> (Image PixelRGBF)
    toRGBF (TexRGBF glimg) = unGLTexture glimg
    toRGBF _ = error "image conversion not possible"




mergeCube :: Pixel p => CubeImageLayout -> p -> Cube (Image p) -> Image p
mergeCube orient background cube =
    generateImage (\x y -> extractImages $ V2 x y) w h
    where
    extractImages p
        | (regions^.to cubeFaceRight)  `containsPoint` p = extract p (regions^.to cubeFaceRight.xy1)  (cubeFaceRight cube)
        | (regions^.to cubeFaceLeft)   `containsPoint` p = extract p (regions^.to cubeFaceLeft.xy1)   (cubeFaceLeft cube)
        | (regions^.to cubeFaceTop)    `containsPoint` p = extract p (regions^.to cubeFaceTop.xy1)    (cubeFaceTop cube)
        | (regions^.to cubeFaceBottom) `containsPoint` p = extract p (regions^.to cubeFaceBottom.xy1) (cubeFaceBottom cube)
        | (regions^.to cubeFaceFront)  `containsPoint` p = extract p (regions^.to cubeFaceFront.xy1)  (cubeFaceFront cube)
        | (regions^.to cubeFaceBack)   `containsPoint` p = extract p (regions^.to cubeFaceBack.xy1)   (cubeFaceBack cube)
        | otherwise = background
    extract p offset img =
        let V2 x y = p - offset
        in pixelAt img x y
    size@(V2 w h)     = faceSize * targetFactors
    faceSize   = V2 (imageWidth $ cubeFaceRight cube) (imageHeight $ cubeFaceRight cube) - 1
    regions    = fmap (\r -> round <$> r `rescale` (fromIntegral <$> size)) (imageRegions orient)
    targetFactors = case orient of
        HorizontalCross -> V2 4 3
        VerticalCross -> V2 3 4
        Strip -> V2 6 1


matchLevelAndSide :: FilePath -> (Int,GL.TextureTargetCubeMapFace)
matchLevelAndSide f =
    let sub :: String
        sub = (fpToString f) =~ (asString "_m[[:digit:]]{1,2}_c[[:digit:]]{1,2}\\.")
        nums :: [Int]
        nums = (concatMap . map) read (sub =~ (asString "[[:digit:]]{1,2}") :: [[String]])
        toCubeSide i = toList glCubeFaces !! i
    in (nums!!0, toCubeSide $ nums !! 1)
