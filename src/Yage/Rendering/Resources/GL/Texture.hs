{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  , textureTarget
  , textureDimension
  , textureLevel
  , textureGL
  -- * Creation
  , createTexture2D
  , createTexture2DImage
  -- * Resize
  , resizeTexture2D
  -- * Binding
  , bindTexture
  , bindTextures
  ) where

import           Yage.Lens
import           Yage.Math         hiding (unit)
import           Yage.Prelude
import           Yage.Rendering.GL as GL

import           Data.Data
import           Yage.Rendering.Resources.GL.Base
import           Yage.Resources
import           Yage.Geometry.D2.Rectangle

import           Codec.Picture                   as Img
import           Codec.Picture.Types             as Img

import           Quine.Cubemap                   as Img
import           Quine.Image                     as Img
import           Quine.MipmapChain               as Img

import           Quine.GL.Framebuffer            as Img
import qualified Quine.GL.Texture                as GL
import           Quine.StateVar


data TextureDimension = Texture1D Int | Texture2D Int Int | Texture3D Int Int Int
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data Texture a = Texture
  { _textureTarget    :: GL.TextureTarget
  , _textureDimension :: TextureDimension
  , _textureLevel     :: GL.MipmapLevel
  , _textureGL        :: (Slot GL.Texture)
  } deriving (Typeable,Generic)

makeLenses ''Texture

-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: (Image2D i, GetRectangle i Int) => GL.TextureTarget -> i -> YageResource (Texture a)
createTexture2DImage target img = throwWithStack $ do
  texSlot <- mkSlot glResource
  tex     <- readSlotResource texSlot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  store img target
  return $ Texture target (Texture2D w h) 1 texSlot
 where
  V2 w h = img^.asRectangle.xy2

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px. ImageFormat px => GL.TextureTarget -> Int -> Int -> YageResource (Texture px)
createTexture2D target w h = throwWithStack $ do
  texSlot    <- mkSlot glResource
  tex     <- readSlotResource texSlot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  glTexStorage2D target 1 (internalFormat (Proxy::Proxy px)) (fromIntegral w) (fromIntegral h)
  return $ Texture target (Texture2D w h) 1 texSlot

resizeTexture2D :: forall px m. (MonadResource m, ImageFormat px) => Texture px -> Int -> Int -> m (Texture px)
resizeTexture2D (Texture target _ level texSlot) w h = throwWithStack $ do
  texSlot $= glResource
  obj <- get texSlot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= obj
  glTexStorage2D target level (internalFormat (Proxy::Proxy px)) (fromIntegral w) (fromIntegral h)
  return $ Texture target (Texture2D w h) level texSlot

bindTexture:: (MonadResource m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture px) -> m ()
bindTexture target st mtex = throwWithStack $ do
  unit <- liftIO $ get st
  bindTextures target [(fromIntegral unit, mtex)]

bindTextures:: MonadResource m => GL.TextureTarget -> [(Int32, Maybe (Texture px))] -> m ()
bindTextures target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= fromIntegral unit
  tex <- maybe (return def) (get . view textureGL) mtex
  GL.boundTexture target 0 $= tex
