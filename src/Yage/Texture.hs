module Yage.Texture
    ( module Yage.Texture
    , module Yage.Rendering.Resources
    ) where

import Yage.Prelude
import Yage.Rendering.Textures
import Yage.Rendering.Resources


loadTexture2D :: MonadIO m => FilePath -> m Texture
loadTexture2D filepath = io $ do
    eImg <- (fromDynamic =<<) <$> readImage (fpToString filepath)
    case eImg of
        Left err    -> error err
        Right img   -> return $ mkTexture (encodeUtf8 $ fpToText filepath) $ Texture2D img

