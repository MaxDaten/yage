{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TupleSections       #-}
module Yage.Rendering.Resources.GL.Texture (
    module Img
  , Texture(..)
  -- * Texture Dimensionality
  , Tex1D(..)
  , Tex2D(..)
  , Tex3D(..)
  , TexCube(..)
  , Texture1D
  , Texture2D
  , Texture3D
  , TextureCube
  , BaseTexture(..)
  , Dimension2D(..)
  , Resizeable2D(resize2D)
  , MipmapLevel
  , textureTarget
  , textureDimension
  , textureLevel
  , textureObject
  -- * Creation
  , createTexture2D
  , createTextureFromImage
  -- * Resize
  , resizeTexture2D
  -- * Binding
  , bindTexture
  , bindTextures
  , bindTextureSamplers
  ) where

import           Yage.Lens         hiding (levels)
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
import           Quine.GL.Texture                (MipmapLevel)
import           Quine.GL.Sampler                as GL
import           Quine.GL.Object
import           Quine.StateVar


data Tex1D   = Tex1D Int deriving (Show,Eq,Ord,Data,Typeable,Generic)
data Tex2D   = Tex2D Int Int deriving (Show,Eq,Ord,Data,Typeable,Generic)
data Tex3D   = Tex3D Int Int Int deriving (Show,Eq,Ord,Data,Typeable,Generic)
data TexCube = TexCube Int Int deriving (Show,Eq,Ord,Data,Typeable,Generic)

type Texture1D = Texture Tex1D
type Texture2D = Texture Tex2D
type Texture3D = Texture Tex3D
type TextureCube = Texture TexCube

data Texture d a = Texture
  { _textureTarget    :: !GL.TextureTarget
  , _textureDimension :: !d
  , _textureLevel     :: !GL.MipmapLevel
  , _textureObject    :: !(IORef GL.Texture)
  } deriving (Typeable,Generic)

instance Show d => Show (Texture d px) where
  show Texture{..} =
    showString "Texture { " .
    showString "textureTarget = " . showString (GL.showTextureTarget _textureTarget) .
    showString ", textureDimension = ". shows _textureDimension .
    showString ", textureLevel = " . shows _textureLevel $
    " }"

makeLenses ''Texture

class BaseTexture t d | t -> d where
  baseTarget :: t -> GL.TextureTarget
  baseDimension :: t -> d
  levels :: t -> GL.MipmapLevel

class Resizeable2D t where
  resize2D :: MonadIO m => t -> Int -> Int -> m t

instance ImageFormat px => Resizeable2D (Texture Tex2D px) where
  resize2D = resizeTexture2D

class Dimension2D d where
  wh :: Lens' d (V2 Int)

instance Dimension2D Tex2D where
  wh = lens getter setter where
    getter (Tex2D w h) = V2 w h
    setter _ (V2 w h) = Tex2D w h

instance Dimension2D TexCube where
  wh = lens getter setter where
    getter (TexCube w h) = V2 w h
    setter _ (V2 w h) = TexCube w h

-- | Creates a 'Texture' initialized with an image
createTextureFromImage :: (Image2D i, GetRectangle i Int, BaseTexture i d) => i -> YageResource (Texture d a)
createTextureFromImage img = mkAcquire acq free where
  acq = throwWithStack $ do
    obj <- gen
    tex <- Texture target dimension (levels img) <$> newIORef obj
    GL.boundTexture target 0 $= obj
    store img target
    upload img target 0
    return tex
  free tex  = delete =<< (get $ tex^.textureObject)
  target    = baseTarget img
  dimension = baseDimension img

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px d. (ImageFormat px, Dimension2D d) => GL.TextureTarget -> d -> Int -> YageResource (Texture d px)
createTexture2D target d l = mkAcquire acq free where
  acq = Texture target d (fromIntegral l) <$> (newIORef =<< newTextureStorageObj target (fromIntegral l) (d^.wh._x) (d^.wh._y) (Proxy :: Proxy px))
  free tex = delete =<< (get $ tex^.textureObject)

resizeTexture2D :: forall px d m. (ImageFormat px, Dimension2D d, MonadIO m) => Texture d px -> Int -> Int -> m (Texture d px)
resizeTexture2D tex w h = throwWithStack $! do
  new <- newTextureStorageObj (tex^.textureTarget) (tex^.textureLevel) w h (Proxy :: Proxy px)
  delete =<< (liftIO $ atomicModifyIORef' (tex^.textureObject) (new,))
  return $ tex & textureDimension.wh .~ V2 w h

bindTexture:: (MonadIO m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture2D px) -> m ()
bindTexture target st mtex = throwWithStack $! do
  unit <- liftIO $ get st
  bindTextures target [(fromIntegral unit, mtex)]

bindTextures:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Texture d px))] -> m ()
bindTextures target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  obj <- maybe (return def) (get . view (textureObject)) mtex
  GL.boundTexture target 0 $= obj

bindTextureSamplers:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Sampler, Texture d px))] -> m ()
bindTextureSamplers target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  obj <- maybe (return def) (get . view (_2.textureObject)) mtex
  GL.boundTexture target 0 $= obj
  GL.boundSampler unit $= maybe def (view _1) mtex

instance FramebufferAttachment (Texture Tex1D a) where
  attach (FramebufferTarget target _) p tex = do
    obj <- liftM object $ get (tex^.textureObject)
    glFramebufferTexture1D target p (tex^.textureTarget) obj 0

instance FramebufferAttachment (Texture Tex2D a) where
  attach (FramebufferTarget target _) p tex = do
    obj <- liftM object $ get (tex^.textureObject)
    glFramebufferTexture2D target p (tex^.textureTarget) obj 0

instance FramebufferAttachment (Texture Tex3D a) where
  attach (FramebufferTarget target _) p tex = do
    obj <- liftM object $ get (tex^.textureObject)
    glFramebufferTexture3D target p (tex^.textureTarget) obj 0 0


newTextureStorageObj :: MonadIO m => ImageFormat px => GL.TextureTarget -> GL.MipmapLevel -> Int -> Int -> Proxy px -> m GL.Texture
newTextureStorageObj t l w h p = throwWithStack $! do
  tex  <- gen
  throwWithStack $ GL.boundTexture t GL_TEXTURE_BINDING_2D $= tex
  throwWithStack $ glTexStorage2D t l (internalFormat p) (fromIntegral w) (fromIntegral h)
  return $ tex

instance (BaseTexture i Tex2D) => BaseTexture (Cubemap i) TexCube where
  baseTarget _ = GL_TEXTURE_CUBE_MAP
  baseDimension = liftToCube2D . baseDimension . faceRight
  levels _ = 1

instance BaseTexture (Image a) Tex2D where
  baseTarget _ = GL_TEXTURE_2D
  baseDimension (Image w h _) = Tex2D w h
  levels _ = 1

instance BaseTexture DynamicImage Tex2D where
  baseTarget _ = GL_TEXTURE_2D
  baseDimension img = Tex2D (dynamicMap imageWidth img) (dynamicMap imageHeight img)
  levels _ = 1

instance BaseTexture (MipmapChain DynamicImage) Tex2D where
  baseTarget = baseTarget . mipMapBase
  baseDimension = baseDimension . mipMapBase
  levels = fromIntegral . maxMipMapLevel

instance BaseTexture (MipmapChain (Image a)) Tex2D where
  baseTarget = baseTarget . mipMapBase
  baseDimension = baseDimension . mipMapBase
  levels = fromIntegral . maxMipMapLevel

instance BaseTexture i Tex2D => BaseTexture (MipmapChain (Cubemap i)) TexCube where
  baseTarget = baseTarget . mipMapBase
  baseDimension = baseDimension . mipMapBase
  levels = fromIntegral . maxMipMapLevel

instance GetRectangle (Texture2D px) Int where
  asRectangle = textureDimension.to (\(Tex2D w h) -> Rectangle 0 (V2 w h))

instance GetRectangle (TextureCube px) Int where
  asRectangle = textureDimension.to (\(TexCube w h) -> Rectangle 0 (V2 w h))

instance GetRectangle i Int => GetRectangle (Cubemap i) Int where
  asRectangle = to (\Cubemap{faceRight} -> faceRight^.asRectangle)

instance GetRectangle i Int => GetRectangle (MipmapChain i) Int where
  asRectangle = to (\mips -> (mipMapBase mips)^.asRectangle)

liftToCube2D :: Tex2D -> TexCube
liftToCube2D (Tex2D w h) = TexCube w h
{-# INLINE liftToCube2D #-}
