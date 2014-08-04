module Yage.Texture
    ( module Yage.Texture
    , module Yage.Rendering.Resources
    , module Atlas
    ) where

import Yage.Prelude
import Yage.Rendering.Textures
import Yage.Rendering.Resources
import Yage.Texture.Atlas as Atlas


loadTexture2D :: MonadIO m => FilePath -> m Texture
loadTexture2D filepath = io $ do
    eImg <- (fromDynamic =<<) <$> readImage (fpToString filepath)
    print $ debugString <$> eImg
    case eImg of
        Left err    -> error err
        Right img   -> return $ mkTexture (encodeUtf8 $ fpToText filepath) $ Texture2D img

