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

data Texture a = Texture
  { _textureTarget    :: GL.TextureTarget
  , _textureDimension :: TextureDimension
  , _textureLevel     :: GL.MipmapLevel
  , _textureGL        :: (Slot GL.Texture)
  } deriving (Typeable,Generic)

makeLenses ''Texture

-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: (Image2D i, GetRectangle i Int) => GL.TextureTarget -> i -> YageResource (Texture a)
createTexture2DImage target img = do
  slot <- mkSlot glResource
  tex     <- readSlotResource slot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  store img target
  return $ Texture target (Texture2D width height) 1 slot
 where
  V2 width height = img^.asRectangle.xy2

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px. ImageFormat px => GL.TextureTarget -> Int -> Int -> YageResource (Texture px)
createTexture2D target width height = do
  slot    <- mkSlot glResource
  tex     <- readSlotResource slot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex
  glTexStorage2D target 1 (internalFormat (Proxy::Proxy px)) (fromIntegral width) (fromIntegral height)
  return $ Texture target (Texture2D width height) 1 slot

resizeTexture2D :: forall px m. (MonadResource m, ImageFormat px) => Texture px -> Int -> Int -> m (Texture px)
resizeTexture2D tex@(Texture target _ level slot) width height = do
  slot $= glResource
  obj <- get slot
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= obj
  glTexStorage2D target level (internalFormat (Proxy::Proxy px)) (fromIntegral width) (fromIntegral height)
  return $ Texture target (Texture2D width height) level slot

bindTexture:: (MonadResource m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture px) -> m ()
bindTexture target st mtex = do
  unit <- liftIO $ get st
  GL.activeTexture $= fromIntegral unit
  tex <- maybe (return def) (get . view textureGL) mtex
  GL.boundTexture target 0 $= tex
