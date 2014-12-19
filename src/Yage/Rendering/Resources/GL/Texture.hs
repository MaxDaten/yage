{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  , createTexture2D
  , createTexture2DImage
  , resizeTexture2D
  , bindTexture
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math
import           Yage.Rendering.GL               as GL

import           Data.Data
import           Foreign.Ptr
import           Yage.Rendering.Resources.GL.Base
import           Yage.Wire.Resources
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

data Texture a = Texture GL.TextureTarget TextureDimension GL.Texture
  deriving (Typeable,Generic)


-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: (Image2D i, GetRectangle i Int) => GL.TextureTarget -> i -> Acquire (Texture a)
createTexture2DImage target img = do
  tex <- glResource
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  store img target
  return $ Texture target (Texture2D width height) tex
 where
  V2 width height = img^.asRectangle.xy2

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px. ImageFormat px => GL.TextureTarget -> Int -> Int -> Acquire (Texture px)
createTexture2D target width height = do
  tex <- glResource
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  glTexStorage2D target 1 (internalFormat (Proxy::Proxy px)) (fromIntegral width) (fromIntegral height)
  return $ Texture target (Texture2D width height) tex

resizeTexture2D :: (MonadIO m, ImageFormat a) => Texture a -> Int -> Int -> m (Texture a)
resizeTexture2D tex@(Texture target _ obj) width height = undefined
-- do
--   GL.boundTexture target GL_TEXTURE_BINDING_2D $= obj
--   glTexImage2D target 0 (internalFormat tex) (fromIntegral width) (fromIntegral height) 0 (pixelFormat tex) (pixelType tex) nullPtr
--   return $ Texture target (Texture2D width height) tex

bindTexture :: HasGetter (g Int32) Int32 => GL.TextureTarget -> g Int32 -> SettableStateVar (Maybe (Texture a))
bindTexture target st = SettableStateVar s where
  s mtex = do
    unit <- get st
    GL.activeTexture $= fromIntegral unit
    GL.boundTexture target 0 $= maybe def (\(Texture _ _ obj) -> obj) mtex

instance FramebufferAttachment (Texture a) where
  attach target slot (Texture _ _ obj) = attach target slot obj
