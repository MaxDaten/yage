{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns      #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  , BaseTextureTarget (baseTextureTarget)
  , Resizeable2D(resize2D)
  , TextureDimension(..)
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
  , bindTextureSamplers
  ) where

import           Yage.Lens
import           Yage.Math         hiding (unit)
import           Yage.Prelude
import           Yage.Rendering.GL as GL

import           Data.Data
import           Yage.Rendering.Resources.GL.Base
import           Yage.Geometry.D2.Rectangle
import           Yage.Resource.YageResource

import           Codec.Picture                   as Img
import           Codec.Picture.Types             as Img

import           Quine.Cubemap                   as Img
import           Quine.Image                     as Img
import           Quine.MipmapChain               as Img

import           Quine.GL.Framebuffer            as Img
import qualified Quine.GL.Texture                as GL
import           Quine.GL.Sampler                as GL
import           Quine.GL.Object
import           Quine.StateVar


data TextureDimension = Texture1D Int | Texture2D Int Int | Texture3D Int Int Int
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

data Texture a = Texture
  { _textureTarget    :: !GL.TextureTarget
  , _textureDimension :: !TextureDimension
  , _textureLevel     :: !GL.MipmapLevel
  , _textureObject    :: !(GL.Texture)
  } deriving (Typeable,Generic)

instance Show (Texture px) where
  show Texture{..} =
    showString "Texture { " .
    showString "textureTarget = " . showString (GL.showTextureTarget _textureTarget) .
    showString ", textureDimension = ". shows _textureDimension .
    showString ", textureLevel = " . shows _textureLevel .
    showString ", textureObject = " . shows _textureObject $
    " }"

makeLenses ''Texture

class BaseTextureTarget t where
  baseTextureTarget :: t -> GL.TextureTarget

class Resizeable2D t where
  resize2D :: MonadIO m => t -> Int -> Int -> m t

instance ImageFormat px => Resizeable2D (Texture px) where
  resize2D = resizeTexture2D

-- | Creates a 'Texture' initialized with an image
createTexture2DImage :: (Image2D i, GetRectangle i Int, BaseTextureTarget i) => i -> YageResource (Texture a)
createTexture2DImage img = mkAcquire acq free where
  acq = throwWithStack $ do
    tex <- Texture (baseTextureTarget img) (Texture2D w h) 1 <$> gen
    GL.boundTexture (baseTextureTarget img) GL_TEXTURE_BINDING_2D $= tex^.textureObject
    store img (baseTextureTarget img)
    upload img (baseTextureTarget img) 0
    return tex
  free tex = delete (tex^.textureObject)
  V2 w h = img^.asRectangle.xy2

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px. (ImageFormat px) => GL.TextureTarget -> Int -> Int -> YageResource (Texture px)
createTexture2D target w h = mkAcquire acq free where
  acq = Texture target (Texture2D w h) 1 <$> newTextureStorageObj target w h (Proxy :: Proxy px)
  free tex = delete (tex^.textureObject)

resizeTexture2D :: forall px m. (ImageFormat px, MonadIO m) => Texture px -> Int -> Int -> m (Texture px)
resizeTexture2D tex w h = throwWithStack $! do
  delete (tex^.textureObject)
  new <- newTextureStorageObj (tex^.textureTarget) w h (Proxy :: Proxy px)
  return $ tex & textureObject .~ new

bindTexture:: (MonadIO m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture px) -> m ()
bindTexture target st mtex = throwWithStack $! do
  unit <- liftIO $ get st
  bindTextures target [(fromIntegral unit, mtex)]

bindTextures:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Texture px))] -> m ()
bindTextures target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  GL.boundTexture target 0 $= maybe def (view (textureObject)) mtex

bindTextureSamplers:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Sampler, Texture px))] -> m ()
bindTextureSamplers target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  GL.boundTexture target 0 $= maybe def (view $ _2.textureObject) mtex
  GL.boundSampler unit $= maybe def (view _1) mtex

instance FramebufferAttachment (Texture a) where
  attach (FramebufferTarget target _) p tex =
    case (tex^.textureDimension) of
      Texture1D _ -> glFramebufferTexture1D target p (tex^.textureTarget) (tex^.textureObject.to object) 0
      Texture2D _ _ -> glFramebufferTexture2D target p (tex^.textureTarget) (tex^.textureObject.to object) 0
      Texture3D _ _ _ -> glFramebufferTexture3D target p (tex^.textureTarget) (tex^.textureObject.to object) 0 0


newTextureStorageObj :: MonadIO m => ImageFormat px => GL.TextureTarget -> Int -> Int -> Proxy px -> m GL.Texture
newTextureStorageObj t w h p = throwWithStack $! do
  tex  <- gen
  GL.boundTexture t GL_TEXTURE_BINDING_2D $= tex
  glTexStorage2D t 1 (internalFormat p) (fromIntegral w) (fromIntegral h)
  return $ tex

instance (Image2D i) => BaseTextureTarget (Cubemap i) where
  baseTextureTarget _ = GL_TEXTURE_CUBE_MAP

instance BaseTextureTarget (Image a) where
  baseTextureTarget _ = GL_TEXTURE_2D

instance BaseTextureTarget DynamicImage where
  baseTextureTarget _ = GL_TEXTURE_2D

instance (BaseTextureTarget i) => BaseTextureTarget (MipmapChain i) where
  baseTextureTarget = baseTextureTarget

instance GetRectangle i Int => GetRectangle (Cubemap i) Int where
  asRectangle = to (\Cubemap{faceRight} -> faceRight^.asRectangle)

instance GetRectangle i Int => GetRectangle (MipmapChain i) Int where
  asRectangle = to (\mips -> (mipMapBase mips)^.asRectangle)
