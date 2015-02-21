{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
module Yage.Rendering.Resources.GL.SparseTexture
 ( -- * Texture Formats
   virtualPageSize3D
 , virtualPageSize2D
 , virtualPageSize1D
 , virtualPageSizes3D
 , virtualPageSizes2D
 , maxSparseSize3D
 , maxSparseSize
 , maxSparseArrayLayers
 , getInternalFormat
 , getInternalFormatv
 -- * Simple GL Wrapper
 , getVirtualPageSizeX
 , getVirtualPageSizeY
 , getVirtualPageSizeZ
 , getVirtualPageSize3D
 , getVirtualPageSize2D
 , getVirtualPageSize1D
 -- * Re-Exports
 , module ARBSparse
 ) where

import GHC.TypeLits
import Graphics.GL.Ext.ARB.SparseTexture as ARBSparse
import Graphics.GL (glGetIntegerv)
import Graphics.GL.Types
import Linear.V
import qualified Quine.GL.InternalFormat as GL
import Quine.GL.Pixel (InternalFormat)
import Quine.GL.Texture (TextureTarget)
import Yage.Lens
import Yage.Math
import Yage.Prelude
import Foreign.Marshal.Alloc
import Foreign.Storable
import Yage.Rendering.Resources.GL.Texture
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- * Checks for 'Yage.Rendering.Resources.GL.Textures'
--------------------------------------------------------------------------------

virtualPageSize3D :: forall px d m. (MonadIO m, ImageFormat px, Dimension3D d) => Texture d px -> m (V3 Int)
virtualPageSize3D tex = getVirtualPageSize3D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

virtualPageSize2D :: forall px d m. (MonadIO m, ImageFormat px, Dimension2D d) => Texture d px -> m (V2 Int)
virtualPageSize2D tex = getVirtualPageSize2D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

virtualPageSize1D :: forall px m. (MonadIO m, ImageFormat px) => Texture1D px -> m Int
virtualPageSize1D tex = getVirtualPageSize1D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

-- | The available page size formats. select it with 'texParameteri GL_TEXTURE_3D GL_VIRTUAL_PAGE_SIZE_INDEX_ARB $= index'
virtualPageSizes3D :: forall m px. (ImageFormat px, MonadIO m) => Texture3D px -> m [V3 Int]
virtualPageSizes3D tex = do
  numSizes <- GL.getInternalFormat1 target fmt GL_NUM_VIRTUAL_PAGE_SIZES_ARB
  liftM3 (zipWith3 V3)
    (GL.getInternalFormats target fmt GL_VIRTUAL_PAGE_SIZE_X_ARB numSizes)
    (GL.getInternalFormats target fmt GL_VIRTUAL_PAGE_SIZE_Y_ARB numSizes)
    (GL.getInternalFormats target fmt GL_VIRTUAL_PAGE_SIZE_Z_ARB numSizes)
 where
  fmt = (internalFormat (Proxy::Proxy px))
  target = tex^.textureTarget

-- | The available page size formats. select it with 'texParameteri GL_TEXTURE_3D GL_VIRTUAL_PAGE_SIZE_INDEX_ARB $= index'
virtualPageSizes2D :: forall m px. (ImageFormat px, MonadIO m) => Texture3D px -> m [V2 Int]
virtualPageSizes2D tex = do
  numSizes <- GL.getInternalFormat1 target fmt GL_NUM_VIRTUAL_PAGE_SIZES_ARB
  liftM2 (zipWith V2)
    (GL.getInternalFormats target fmt GL_VIRTUAL_PAGE_SIZE_X_ARB numSizes)
    (GL.getInternalFormats target fmt GL_VIRTUAL_PAGE_SIZE_Y_ARB numSizes)
 where
  fmt = (internalFormat (Proxy::Proxy px))
  target = tex^.textureTarget

-- | Maximum 3D texture image dimension for sparse texture
maxSparseSize3D :: Int
maxSparseSize3D = unsafePerformIO $ alloca $ (>>) <$> glGetIntegerv GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB <*> fmap fromIntegral . peek

-- | Maximum 1D/2D/Rectangle texture image dimension for sparse texture
maxSparseSize :: Int
maxSparseSize = unsafePerformIO $ alloca $ (>>) <$> glGetIntegerv GL_MAX_SPARSE_TEXTURE_SIZE_ARB <*> fmap fromIntegral . peek

-- | Maximum 1D/2D/Rectangle texture image dimension for sparse texture
maxSparseArrayLayers :: Int
maxSparseArrayLayers = unsafePerformIO $ alloca $ (>>) <$> glGetIntegerv GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB <*> fmap fromIntegral . peek

-- | Utilitiy function to perform format queries on concrete textures
getInternalFormat :: forall m d px. (MonadIO m, ImageFormat px) => GLenum -> Texture d px -> m Int
getInternalFormat pname tex = GL.getInternalFormat1 (tex^.textureTarget) (internalFormat (Proxy::Proxy px)) pname

-- | Utilitiy function to perform format queries on concrete textures
getInternalFormatv :: forall m d px (n::Nat). (MonadIO m, ImageFormat px, Dim n) => GLenum -> Texture d px -> m (V n Int)
getInternalFormatv pname tex = GL.getInternalFormatv (tex^.textureTarget) (internalFormat (Proxy::Proxy px)) pname

--------------------------------------------------------------------------------
-- * Simple GL Wrapper
--------------------------------------------------------------------------------

-- | The x size of the virtual page for the given format and target
getVirtualPageSizeX :: MonadIO m => TextureTarget -> InternalFormat -> m Int
getVirtualPageSizeX t i = GL.getInternalFormat1 t i GL_VIRTUAL_PAGE_SIZE_X_ARB

-- | The y size of the virtual page for the given format and target
getVirtualPageSizeY :: MonadIO m => TextureTarget -> InternalFormat -> m Int
getVirtualPageSizeY t i = GL.getInternalFormat1 t i GL_VIRTUAL_PAGE_SIZE_Y_ARB

-- | The y size of the virtual page for the given format and target
getVirtualPageSizeZ :: MonadIO m => TextureTarget -> InternalFormat -> m Int
getVirtualPageSizeZ t i = GL.getInternalFormat1 t i GL_VIRTUAL_PAGE_SIZE_Z_ARB

-- | The y size of the virtual page for the given format and target
getVirtualPageSize3D :: MonadIO m => TextureTarget -> InternalFormat -> m (V3 Int)
getVirtualPageSize3D t i = liftM3 V3 (getVirtualPageSizeX t i) (getVirtualPageSizeY t i) (getVirtualPageSizeZ t i)

-- | The y size of the virtual page for the given format and target
getVirtualPageSize2D :: MonadIO m => TextureTarget -> InternalFormat -> m (V2 Int)
getVirtualPageSize2D t i = liftM2 V2 (getVirtualPageSizeX t i) (getVirtualPageSizeY t i)

-- | The y size of the virtual page for the given format and target
getVirtualPageSize1D :: MonadIO m => TextureTarget -> InternalFormat -> m Int
getVirtualPageSize1D t i = getVirtualPageSizeX t i
