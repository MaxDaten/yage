{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
module Yage.Rendering.Resources.GL.SparseTexture
 ( -- * Texture Formats
   virtualPageSize3D
 , virtualPageSize2D
 , virtualPageSize1D
 , maxSparseTextureSize3D
 , maxSparseTextureSize
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

import Yage.Prelude
import Yage.Math
import Yage.Lens
import Graphics.GL.Ext.ARB.SparseTexture as ARBSparse
import Graphics.GL.Types
import qualified Quine.GL.InternalFormat as GL
import Quine.GL.Texture (TextureTarget)
import Quine.GL.Pixel (InternalFormat)
import Yage.Rendering.Resources.GL.Texture
import GHC.TypeLits
import Linear.V


--------------------------------------------------------------------------------
-- * Checks for 'Yage.Rendering.Resources.GL.Textures'
--------------------------------------------------------------------------------

virtualPageSize3D :: forall px d m. (MonadIO m, ImageFormat px, Dimension3D d) => Texture d px -> m (V3 Int)
virtualPageSize3D tex = getVirtualPageSize3D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

virtualPageSize2D :: forall px d m. (MonadIO m, ImageFormat px, Dimension2D d) => Texture d px -> m (V2 Int)
virtualPageSize2D tex = getVirtualPageSize2D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

virtualPageSize1D :: forall px m. (MonadIO m, ImageFormat px) => Texture1D px -> m Int
virtualPageSize1D tex = getVirtualPageSize1D (tex^.textureTarget) (internalFormat (Proxy::Proxy px))

-- | Maximum 3D texture image dimension for sparse texture
maxSparseTextureSize3D :: forall px d m. (MonadIO m, ImageFormat px, Dimension3D d) => Texture d px -> m Int
maxSparseTextureSize3D = getInternalFormat GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB

-- | Maximum 1D/2D/Rectangle texture image dimension for sparse texture
maxSparseTextureSize :: forall px d m. (MonadIO m, ImageFormat px) => Texture d px -> m Int
maxSparseTextureSize = getInternalFormat GL_MAX_SPARSE_TEXTURE_SIZE_ARB

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
