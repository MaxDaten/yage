{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Yage.Rendering.Resources.GL.TextureFormat
  (
  -- * Color Formats
    PixelRG16F
  , PixelRGB16F
  , PixelRGB11_11_10F
  -- * Depth Formats
  , DepthComponent16
  , DepthComponent24
  , DepthComponent32
  , DepthComponent32F
  , Depth24Stencil8
  , Depth32FStencil8
  -- * Stencil Formats
  , StencilIndex1
  , StencilIndex4
  , StencilIndex8
  , StencilIndex16
  ) where

import           Yage.Prelude
import           Yage.Rendering.GL

import           Codec.Picture
import           Data.Data
import           Data.Word
import           Numeric.Half
import           Quine.Image

-- | Further Pixeltypes beside JuicyPixel's
data PixelRG16F
data PixelRGB16F
data PixelRGB11_11_10F

-- | dangling instances for Pixel instance
deriving instance Eq PixelRG16F
deriving instance Eq PixelRGB16F
deriving instance Eq PixelRGB11_11_10F

instance Pixel PixelRG16F where
  type PixelBaseComponent PixelRG16F = Half

instance ImageFormat PixelRG16F where
  internalFormat _ = GL_RG16F
  pixelFormat    _ = GL_RG
  pixelType      _ = GL_HALF_FLOAT

instance Pixel PixelRGB16F where
  type PixelBaseComponent PixelRGB16F = Half

instance ImageFormat PixelRGB16F where
  internalFormat _ = GL_RGB16F
  pixelFormat    _ = GL_RGB
  pixelType      _ = GL_HALF_FLOAT

instance Pixel PixelRGB11_11_10F where
  -- TODO better base component
  type PixelBaseComponent PixelRGB11_11_10F = Float

instance ImageFormat PixelRGB11_11_10F where
  internalFormat _ = GL_R11F_G11F_B10F
  pixelFormat    _ = GL_RGB
  pixelType      _ = GL_FLOAT

-- | Plain sized internal formats for the phantom types
-- <https://www.opengl.org/registry/doc/glspec45.core.pdf> Table 8.13
newtype DepthComponent16 a   = DepthComponent16 a  deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype DepthComponent24 a   = DepthComponent24 a  deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype DepthComponent32 a   = DepthComponent32 a  deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype DepthComponent32F a  = DepthComponent32F a deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype Depth24Stencil8 a    = Depth24Stencil8 a   deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype Depth32FStencil8 a   = Depth32FStencil8 a  deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype StencilIndex1 a      = StencilIndex1 a     deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype StencilIndex4 a      = StencilIndex4 a     deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype StencilIndex8 a      = StencilIndex8 a     deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)
newtype StencilIndex16 a     = StencilIndex16 a    deriving (Num,Eq,Ord,Show,Data,Typeable,Generic)

-- TODO : Full Pixel implementation (important for upload and download)
instance Pixel a => Pixel (DepthComponent16 a) where
  type PixelBaseComponent (DepthComponent16 a) = PixelBaseComponent a

instance Pixel a => Pixel (DepthComponent24 a) where
  type PixelBaseComponent (DepthComponent24 a) = PixelBaseComponent a

instance Pixel a => Pixel (DepthComponent32 a) where
  type PixelBaseComponent (DepthComponent32 a) = PixelBaseComponent a

instance Pixel a => Pixel (DepthComponent32F a) where
  type PixelBaseComponent (DepthComponent32F a) = PixelBaseComponent a

instance Pixel a => Pixel (Depth24Stencil8 a) where
  type PixelBaseComponent (Depth24Stencil8 a) = PixelBaseComponent a

instance Pixel a => Pixel (Depth32FStencil8 a) where
  type PixelBaseComponent (Depth32FStencil8 a) = PixelBaseComponent a

instance Pixel a => Pixel (StencilIndex1 a) where
  type PixelBaseComponent (StencilIndex1 a) = PixelBaseComponent a

instance Pixel a => Pixel (StencilIndex4 a) where
  type PixelBaseComponent (StencilIndex4 a) = PixelBaseComponent a

instance Pixel a => Pixel (StencilIndex8 a) where
  type PixelBaseComponent (StencilIndex8 a) = PixelBaseComponent a

instance Pixel a => Pixel (StencilIndex16 a) where
  type PixelBaseComponent (StencilIndex16 a) = PixelBaseComponent a

-- <https://www.opengl.org/wiki/Image_Format#Required_formats>

instance ImageFormat (DepthComponent16 Float) where
  internalFormat _ = GL_DEPTH_COMPONENT16
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_FLOAT

instance ImageFormat (DepthComponent24 Float) where
  internalFormat _ = GL_DEPTH_COMPONENT24
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_FLOAT

instance ImageFormat (DepthComponent32 Float) where
  internalFormat _ = GL_DEPTH_COMPONENT32
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_FLOAT

instance ImageFormat (DepthComponent16 Word8) where
  internalFormat _ = GL_DEPTH_COMPONENT16
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (DepthComponent24 Word8) where
  internalFormat _ = GL_DEPTH_COMPONENT24
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (DepthComponent32 Word8) where
  internalFormat _ = GL_DEPTH_COMPONENT32
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (DepthComponent32F Float) where
  internalFormat _ = GL_DEPTH_COMPONENT32F
  pixelFormat    _ = GL_DEPTH_COMPONENT
  pixelType      _ = GL_FLOAT

instance ImageFormat (Depth24Stencil8 Float) where
  internalFormat _ = GL_DEPTH24_STENCIL8
  pixelFormat    _ = GL_DEPTH_STENCIL
  pixelType      _ = GL_FLOAT

instance ImageFormat (Depth32FStencil8 Float) where
  internalFormat _ = GL_DEPTH32F_STENCIL8
  pixelFormat    _ = GL_DEPTH_STENCIL
  pixelType      _ = GL_FLOAT

instance ImageFormat (StencilIndex1 Word8) where
  internalFormat _ = GL_STENCIL_INDEX1
  pixelFormat    _ = GL_STENCIL_INDEX
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (StencilIndex4 Word8) where
  internalFormat _ = GL_STENCIL_INDEX4
  pixelFormat    _ = GL_STENCIL_INDEX
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (StencilIndex8 Word8) where
  internalFormat _ = GL_STENCIL_INDEX8
  pixelFormat    _ = GL_STENCIL_INDEX
  pixelType      _ = GL_UNSIGNED_BYTE

instance ImageFormat (StencilIndex16 Word16) where
  internalFormat _ = GL_STENCIL_INDEX16
  pixelFormat    _ = GL_STENCIL_INDEX
  pixelType      _ = GL_UNSIGNED_BYTE
