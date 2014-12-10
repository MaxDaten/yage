{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  , createTexture2D
  , createTexture2DImage
  , resizeTexture2D
  ) where

import           Yage.Prelude
import           Yage.Rendering.GL

import           Data.Data
import           Foreign.Ptr
import           Yage.Rendering.Resources.GL.Base

import           Codec.Picture                   as Img
import           Codec.Picture.Types             as Img

import           Quine.Cubemap                   as Img
import           Quine.Image                     as Img
import           Quine.MipmapChain               as Img

import           Quine.GL.Framebuffer            as Img
import qualified Quine.GL.Texture                as GL
import           Quine.StateVar


data TextureDimension = Texture1D | Texture2D | Texture3D
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data Texture a = Texture GL.TextureTarget TextureDimension GL.Texture
  deriving (Show,Eq,Ord,Data,Typeable,Generic)


-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: Image2D i => GL.TextureTarget -> i -> Acquire (Texture a)
createTexture2DImage target img = do
  tex <- glResource
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  upload img target 0
  return $ Texture target Texture2D tex

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from a proxy
createTexture2D :: ImageFormat px => GL.TextureTarget -> Int -> Int -> Acquire (Texture px)
createTexture2D target width height = do
  tex <- glResource
  resizeTexture2D (Texture target Texture2D tex) width height


resizeTexture2D :: (MonadIO m, ImageFormat a) => Texture a -> Int -> Int -> m (Texture a)
resizeTexture2D tex@(Texture target _ obj) width height = do
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= obj
  glTexImage2D target 0 (internalFormat tex) (fromIntegral width) (fromIntegral height) 0 (pixelFormat tex) (pixelType tex) nullPtr
  return tex

instance FramebufferAttachment (Texture a) where
  attach target slot (Texture _ _ obj) = attach target slot obj
