{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  , textureTarget
  , textureDimension
  , textureLevel
  , textureObject
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
import           Quine.GL.Object
import           Quine.StateVar


data TextureDimension = Texture1D Int | Texture2D Int Int | Texture3D Int Int Int
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data Texture a = Texture
  { _textureTarget    :: !GL.TextureTarget
  , _textureDimension :: !TextureDimension
  , _textureLevel     :: !GL.MipmapLevel
  , _textureObject    :: !GL.Texture
  } deriving (Show,Typeable,Generic)

-- type Texture a = Slot (TextureData a)

makeLenses ''Texture

-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: (Image2D i, GetRectangle i Int) => GL.TextureTarget -> i -> YageResource (Texture a)
createTexture2DImage target img = throwWithStack $ do
  tex <- Texture target (Texture2D w h) 1 <$> glResource
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex^.textureObject
  store img target
  return tex
 where
  V2 w h = img^.asRectangle.xy2

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px. ImageFormat px => GL.TextureTarget -> Int -> Int -> YageResource (Texture px)
createTexture2D target w h = throwWithStack $! do
  tex  <- Texture target (Texture2D w h) 1 <$> glResource
  GL.boundTexture target GL_TEXTURE_BINDING_2D $= tex^.textureObject
  glTexStorage2D target 1 (internalFormat (Proxy::Proxy px)) (fromIntegral w) (fromIntegral h)
  return $ tex

resizeTexture2D :: forall px. (ImageFormat px) => Texture px -> Int -> Int -> YageResource (Texture px)
resizeTexture2D tex w h = throwWithStack $! do
  t <- createTexture2D (tex^.textureTarget) w h :: YageResource (Texture px)
  return $ t & textureLevel .~ tex^.textureLevel

bindTexture:: (MonadResource m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture px) -> m ()
bindTexture target st mtex = throwWithStack $! do
  unit <- liftIO $ get st
  bindTextures target [(fromIntegral unit, mtex)]

bindTextures:: MonadResource m => GL.TextureTarget -> [(Int32, Maybe (Texture px))] -> m ()
bindTextures target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= fromIntegral unit
  GL.boundTexture target 0 $= maybe def (view textureObject) mtex

instance FramebufferAttachment (Texture a) where
  attach (FramebufferTarget target _) p tex =
    case (tex^.textureDimension) of
      Texture1D _ -> glFramebufferTexture1D target p (tex^.textureTarget) (tex^.textureObject.to object) 0
      Texture2D _ _ -> glFramebufferTexture2D target p (tex^.textureTarget) (tex^.textureObject.to object) 0
      Texture3D _ _ _ -> glFramebufferTexture3D target p (tex^.textureTarget) (tex^.textureObject.to object) 0 0
