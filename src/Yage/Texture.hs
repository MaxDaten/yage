{-# LANGUAGE NamedFieldPuns #-}
module Yage.Texture
    ( module Yage.Texture
    , module Yage.Rendering.Resources
    ) where

import Yage.Prelude
import Yage.Lens

import Data.Traversable
import Filesystem.Path

import Yage.Rendering.Textures
import Yage.Rendering.Resources
import Yage.Material

data TextureLoadError = TextureLoadError String deriving ( Show, Typeable )
instance Exception TextureLoadError

loadImage :: MonadIO m => FilePath -> m TextureImage
loadImage filepath = io $ do
    eImg <- (fromDynamic =<<) <$> readImage (fpToString filepath)
    case eImg of
        Left err    -> throwM $ TextureLoadError $ "Yage.Texture.loadTexture2D: " ++ err
        Right img   -> return img

-- | fix SRGB drop
writeTextureImage :: MonadIO m => FilePath -> TextureImage -> m ()
writeTextureImage fp = io . write fp
    where
    write file (TexY8 (GLTexture img))    = writePng (fpToString $ replaceExtension file "png") img
    write file (TexYF (GLTexture img))    = writeHDR (fpToString $ replaceExtension file "hdr") (promoteImage img)
    write file (TexRGB8 (GLTexture img))  = writePng (fpToString $ replaceExtension file "png") img
    write file (TexRGBF (GLTexture img))  = writeHDR (fpToString $ replaceExtension file "hdr") img
    write file (TexRGBA8 (GLTexture img)) = writePng (fpToString $ replaceExtension file "png") img
    write file (TexSRGB8 (GLTexture img)) = writePng (fpToString $ replaceExtension file "png") (convertImage img :: Image PixelRGB8)


loadTexture2D :: MonadIO m => FilePath -> m Texture
loadTexture2D filepath =
    liftM (mkTexture2D (encodeUtf8 $ fpToText filepath)) $ loadImage filepath


-- | extracts the `TextureImages` from the `Cube` `Texture` fields and creates
-- a new Texture with Cube `TextureData`
cubeTextureToTexture :: ByteString -> Cube Texture -> Texture
cubeTextureToTexture ident cubeTexs =
    mkTextureCubeMip ident $ mkMipMapChain ( unsafeHead cubeImgs ) ( unsafeTail cubeImgs )
    where
    getTextureImg :: TextureData -> [TextureImage]
    getTextureImg (Texture2D img) = toList img
    getTextureImg _ = throwM $ TextureLoadError "Yage.Texture.requestResources: invalid TextureData"

    cubeImgs :: [Cube TextureImage]
    cubeImgs = sequenceA $ cubeTexs & mapped %~ ( \tex -> getTextureImg $ tex^.textureData )


defaultCubeMap :: Texture
defaultCubeMap = cubeTextureToTexture "DefaultCubeMap" $ mkTexture2D "" <$>
    Cube   { cubeFaceRight  = TexRGB8 `pxTexture` red
           , cubeFaceLeft   = TexRGB8 `pxTexture` magenta
           , cubeFaceTop    = TexRGB8 `pxTexture` lime
           , cubeFaceBottom = TexRGB8 `pxTexture` yellow
           , cubeFaceFront  = TexRGB8 `pxTexture` blue
           , cubeFaceBack   = TexRGB8 `pxTexture` cyan
           }
