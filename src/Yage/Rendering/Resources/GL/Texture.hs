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
  , Dimension3D(..)
  , Resizeable2D(resize2D)
  , MipmapLevel
  , textureTarget
  , textureDimension
  , textureLevel
  , textureObject
  -- * Creation
  , createTexture2D
  , createTexture3D
  , createTexture3DWithSetup
  , createTextureFromImage
  -- * Resize
  , resizeTexture2D
  -- * Binding
  , bindTexture
  , bindTextures
  , bindTextureSamplers
  , withTextureBound
  , withMappedTexture
  , withPixels3D
  ) where

import           Yage.Lens         hiding (levels, coerce)
import           Yage.Math         hiding (unit)
import           Yage.Prelude
import           Yage.Rendering.GL as GL

import           Data.Data
import           Data.Foldable                   (foldr1)
import           Data.Vector.Storable            (unsafeFromForeignPtr0, convert)
import           Foreign.Ptr
import           Foreign.ForeignPtr.Safe
import           Yage.Rendering.Resources.GL.Base
import           Yage.Geometry.D2.Rectangle
import           Yage.Resource.YageResource
import           Codec.Picture                   as Img
import           Codec.Picture.Types             as Img
import           Foreign.Storable

import           Quine.Cubemap                   as Img
import           Quine.Image                     as Img
import           Quine.MipmapChain               as Img

import           Quine.GL.Framebuffer            as Img
import qualified Quine.GL.Texture                as GL
import           Quine.GL.Texture                (MipmapLevel)
import           Quine.GL.Sampler                as GL
import           Quine.GL.Object
import           Quine.GL.Buffer
import           Quine.GL.Pixel
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
  , _textureObject    :: !GL.Texture
  } deriving (Eq,Ord,Data,Typeable,Generic)

makeLenses ''Texture

instance Show d => Show (Texture d px) where
  showsPrec d Texture{..} = showParen (d>10) $ showString "Texture {" .
    showString "textureTarget = " . showString (GL.showTextureTarget _textureTarget) .
    showString ",textureDimension = " . shows _textureDimension .
    showString ",textureLevel = " . shows _textureLevel .
    showString ",textureObject = " . shows _textureObject .
{--
    showString ",internalFormat = " . shows (internalFormat (Proxy::Proxy px)) .
    showString ",pixelFormat = " . shows (pixelFormat (Proxy::Proxy px)) .
    showString ",pixelType = " . shows (pixelType (Proxy::Proxy px)) .
--}
    showString "}"

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

class Dimension3D d where
  whd :: Lens' d (V3 Int)

instance Dimension3D Tex3D where
  whd = lens getter setter where
    getter (Tex3D w h d) = V3 w h d
    setter _ (V3 w h d) = Tex3D w h d

-- | Creates a 'Texture' initialized with an image
createTextureFromImage :: (Image2D i, GetRectangle i Int, BaseTexture i d) => i -> YageResource (Texture d a)
createTextureFromImage img = mkAcquire acq free where
  acq = throwWithStack $ do
    obj <- gen
    let tex = Texture target dimension (levels img) obj
    GL.boundTexture target 0 $= obj
    store img target
    upload img target 0
    return tex
  free tex  = delete $ tex^.textureObject
  target    = baseTarget img
  dimension = baseDimension img

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture2D :: forall px d. (ImageFormat px, Dimension2D d) => GL.TextureTarget -> d -> Int -> YageResource (Texture d px)
createTexture2D target d l = mkAcquire acq free where
  acq = Texture target d (fromIntegral l) <$> (newTextureStorageObj target (fromIntegral l) (d^.wh._x) (d^.wh._y) (Proxy :: Proxy px))
  free tex = delete (tex^.textureObject)

createTexture3D :: forall px d. (ImageFormat px, Dimension3D d) => GL.TextureTarget -> d -> Int -> YageResource (Texture d px)
createTexture3D target dim l = createTexture3DWithSetup target dim l (const (return ()))

-- | Creates an uninitialized 'Texture' with 'ImageFormat' derived from the result type
createTexture3DWithSetup :: forall px d. (ImageFormat px, Dimension3D d) => GL.TextureTarget -> d -> Int -> (Texture d px -> IO ()) -> YageResource (Texture d px)
createTexture3DWithSetup target dim l ma = mkAcquire acq free where
  free tex = delete (tex^.textureObject)
  acq :: IO (Texture d px)
  acq = do
    obj <- gen
    let tex = Texture target dim (fromIntegral l) obj
    GL.boundTexture target 0 $= obj
    ma tex
    glTexStorage3D target (fromIntegral l) (internalFormat (Proxy :: Proxy px)) w h d
    return $ tex
  V3 w h d = fromIntegral <$> dim^.whd

resizeTexture2D :: forall px d m. (ImageFormat px, Dimension2D d, MonadIO m) => Texture d px -> Int -> Int -> m (Texture d px)
resizeTexture2D tex w h = throwWithStack $! do
  new <- newTextureStorageObj (tex^.textureTarget) (tex^.textureLevel) w h (Proxy :: Proxy px)
  delete $ tex^.textureObject
  return $ tex & textureObject .~ new & textureDimension.wh .~ V2 w h

bindTexture:: (MonadIO m, HasGetter g IO a, Integral a) => GL.TextureTarget -> g -> Maybe (Texture2D px) -> m ()
bindTexture target st mtex = throwWithStack $! do
  unit <- liftIO $ get st
  bindTextures target [(fromIntegral unit, mtex)]

bindTextures:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Texture d px))] -> m ()
bindTextures target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  GL.boundTexture target 0 $= maybe def (view (textureObject)) mtex

bindTextureSamplers:: MonadIO m => GL.TextureTarget -> [(GL.TextureUnit, Maybe (Sampler, Texture d px))] -> m ()
bindTextureSamplers target pairs = throwWithStack $ forM_ pairs $ \(unit,mtex) -> do
  GL.activeTexture $= unit
  GL.boundTexture target 0 $= maybe def (view (_2.textureObject)) mtex
  GL.boundSampler unit $= maybe def (view _1) mtex

newTextureStorageObj :: MonadIO m => ImageFormat px => GL.TextureTarget -> GL.MipmapLevel -> Int -> Int -> Proxy px -> m GL.Texture
newTextureStorageObj t l w h p = throwWithStack $! do
  tex  <- gen
  throwWithStack $ GL.boundTexture t GL_TEXTURE_BINDING_2D $= tex
  throwWithStack $ glTexStorage2D t l (internalFormat p) (fromIntegral w) (fromIntegral h)
  return $ tex

withPixels3D :: forall m px a. (Storable px, ImageFormat px, MonadIO m) => Texture3D px -> (Vector px -> m a) -> m a
withPixels3D tex ma = withTextureBound tex $ do
  fptr <- io $ mallocForeignPtrBytes bytes
  glPixelStorei GL_PACK_ALIGNMENT 1
  io $ withForeignPtr fptr $ glGetnTexImage (tex^.textureTarget) 0 fmt ty (fromIntegral bytes) . castPtr
  r <- ma (convert $ unsafeFromForeignPtr0 fptr len)
  io $ touchForeignPtr fptr
  return r
 where
  target = tex^.textureTarget
  fmt = pixelFormat (Proxy::Proxy px)
  ty  = pixelType (Proxy::Proxy px)
  len = foldr1 (*) $ tex^.textureDimension.whd
  bytes = len * components (pixelFormat (Proxy::Proxy px)) * sizeOf ((undefined :: PixelBaseComponent px))

withTextureBound :: MonadIO m => Texture d px -> m a -> m a
withTextureBound tex ma = do
  GL.boundTexture (tex^.textureTarget) 0 $= (tex^.textureObject)
  r <- ma
  GL.boundTexture (tex^.textureTarget) 0 $= def
  return r

-- | bracket style texture mapping, just read only
-- a better solution: create, capture and map after it
-- https://www.opengl.org/discussion_boards/showthread.php/165780-PBO-glReadPixels-not-so-fast?p=1172482&viewfull=1#post1172482
withMappedTexture :: forall m px a. (MonadIO m, ImageFormat px, Storable px) =>
  Buffer (SVector px) ->
  GLenum ->
  -- ^ 'GL_READ_ONLY' | 'GL_WRITE_ONLY' 'GL_READ_WRITE'
  Texture3D px ->
  (Vector px -> m a) ->
  m a
withMappedTexture storage access tex ma = do
  GL.boundTexture target 0 $= (tex^.textureObject)
  boundBufferAt PixelPackBuffer $= storage
  --r <- ma . flip unsafeFromForeignPtr0 len =<< liftIO . newForeignPtr_ =<< (liftM castPtr $ glMapBufferRange GL_PIXEL_UNPACK_BUFFER 0 (fromIntegral len) GL_MAP_READ_BIT)
  glGetnTexImage target 0 fmt ty (fromIntegral 2048) nullPtr
  fptr <- liftIO $ newForeignPtr_ =<< liftM castPtr (glMapBuffer GL_PIXEL_PACK_BUFFER access)
  a <- ma (convert $ unsafeFromForeignPtr0 fptr len)
  r <- glUnmapBuffer GL_PIXEL_PACK_BUFFER
  when (r /= GL_TRUE) $ error "glUnmapBuffer not successful"
  boundBufferAt PixelPackBuffer   $= def
  GL.boundTexture (tex^.textureTarget) 0 $= def
  return a
 where
  target = tex^.textureTarget
  fmt = pixelFormat (Proxy::Proxy px)
  ty  = pixelType (Proxy::Proxy px)
  len = foldr1 (*) $ tex^.textureDimension.whd
  bytes = len * components (pixelFormat (Proxy::Proxy px))

instance FramebufferAttachment (Texture Tex1D a) where
  attach (FramebufferTarget target _) p tex = glFramebufferTexture1D target p (tex^.textureTarget) (tex^.textureObject.to object) 0

instance FramebufferAttachment (Texture Tex2D a) where
  attach (FramebufferTarget target _) p tex = glFramebufferTexture2D target p (tex^.textureTarget) (tex^.textureObject.to object) 0

instance FramebufferAttachment (Texture Tex3D a) where
  attach (FramebufferTarget target _) p tex = glFramebufferTexture3D target p (tex^.textureTarget) (tex^.textureObject.to object) 0 0

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
