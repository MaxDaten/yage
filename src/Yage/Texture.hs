{-# LANGUAGE NamedFieldPuns #-}
module Yage.Texture
    ( module Yage.Texture
    , module Yage.Rendering.Resources
    ) where

import Yage.Prelude
import Yage.Lens

import Data.Traversable

import Yage.Rendering.Textures
import Yage.Rendering.Resources
import Yage.Rendering.Textures.MipMapChain

data TextureLoadError = TextureLoadError String deriving ( Show, Typeable )
instance Exception TextureLoadError

loadTexture2D :: MonadIO m => FilePath -> m Texture
loadTexture2D filepath = io $ do
    eImg <- (fromDynamic =<<) <$> readImage (fpToString filepath)
    case eImg of
        Left err    -> throwM $ TextureLoadError $ "Yage.Texture.loadTexture2D" ++ err
        Right img   -> return $ mkTexture2D (encodeUtf8 $ fpToText filepath) img


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
