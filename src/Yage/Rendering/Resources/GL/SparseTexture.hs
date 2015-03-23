{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}
module Yage.Rendering.Resources.GL.SparseTexture
 ( SparseTexture(..)
 , sparseTexture
 , pageMask
 , pageSizes
 , pagesIn
 , PageMask
 , Pages
 , PageIndex
 -- * Alias
 , SparseTexture3D
 , SparseTexture2D
 , PageMask3D
 , PageMask2D
 -- * Generation
 , genSparseTexture3D
 -- * Updating
 , updateSparseTexture
 , commitPage
 , syncPages
 -- * Reexports
 , module Internal
 ) where

import Yage.Prelude hiding (mask)
import Yage.Lens
import Yage.Math
import Yage.Resources
import Yage.Rendering.GL
import Yage.Rendering.RenderTarget
import Yage.Rendering.Resources.GL
import Yage.Rendering.Resources.GL.SparseTexture.Internal as Internal
import Data.Foldable (foldr1)
import GHC.Real (lcm)
import Control.Monad.State               (MonadState, execStateT, StateT)
import Data.List (findIndex,(!!))
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Quine.GL.Texture hiding (Texture)
import Quine.StateVar

type PageMask d = Texture d PixelR8UI
type PageIndex = V3 Int
type Pages = Set PageIndex
type SparseTexture3D = SparseTexture Tex3D
type SparseTexture2D = SparseTexture Tex2D
type PageMask3D = PageMask Tex3D
type PageMask2D = PageMask Tex2D

data SparseTexture d px = SparseTexture
  { _sparseTexture   :: Texture d px
  , _pageMask        :: PageMask d
  , _pageSizes       :: V3 Int
  , _pagesIn         :: Pages
  -- ^ committed pages of the sparse texture
  , _mipmapClaims    :: MultiSet (Int,PageIndex)
  -- ^ multiple base level pages can claim the same mipmap page
  -- because page sizes are not divideable.
  -- so we have to ref counting mipmap pages
  } deriving (Show,Ord,Eq,Generic)

makeLenses ''SparseTexture


-- | Generates a 3D Texture with 'GL_TEXTURE_SPARSE_ARB' enabled
-- according to the specs a fresh texture is completly virtual
-- use '@commitPage' to commit or decommit pages
genSparseTexture3D :: (ImageFormat px, Pixel px) => Int -> Int -> Int -> Int -> YageResource (SparseTexture3D px)
genSparseTexture3D w h d l
 | not gl_ARB_sparse_texture = error "ARB_sparse_texture not available"
 | otherwise = do
  tex <- createTexture3DWithSetup GL_TEXTURE_3D (Tex3D w h d) l $ \t -> do
    fmtIdx <- selectPageFormat t
    printf "Selected Page Format: %s\n" (show fmtIdx)
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_BASE_LEVEL $= 0
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAX_LEVEL  $= fromIntegral (l-1)
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_SPARSE_ARB $= GL_TRUE
    texParameteri GL_TEXTURE_3D GL_VIRTUAL_PAGE_SIZE_INDEX_ARB $= (fromIntegral $ fst fmtIdx) -- on my machine 128x128x1 - on gtx 980 32x32x16

  (_idx, fmt) <- selectPageFormat tex
  (mask, selectedPageSize) <- genPageMask tex fmt
  return $ SparseTexture tex mask selectedPageSize mempty mempty
 where
  selectPageFormat :: (MonadIO m, Functor m, ImageFormat px) => Texture3D px -> m (Int, V3 Int)
  selectPageFormat tex = do
    fmts <- traceShowId <$> virtualPageSizes3D tex
    case (findIndex  ((==) 1 . view _z)) fmts of
      Nothing     -> return (0,unsafeHead fmts) -- error "GL_TEXTURE_3D requires a tile depth == 1, no matching format found"
      Just fmtIdx -> return $ (fmtIdx, fmts!!fmtIdx)


genPageMask :: forall px. ImageFormat px => Texture3D px -> V3 Int -> YageResource (PageMask3D, V3 Int)
genPageMask baseBuff pageSize = do
  -- select the common least multiple to select cubic page sizes
  let lcmPageSize = traceShowId <$> pure $ foldr1 lcm pageSize :: V3 Int
  tex <- createTexture3DWithSetup GL_TEXTURE_3D (calcMaskSize lcmPageSize) 1 $ \_ -> do
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
  return (tex, lcmPageSize)
 where
  calcMaskSize (V3 x y z) = (baseBuff^.textureDimension)
    & whd._x %~ (`div` x)
    & whd._y %~ (`div` y)
    & whd._z %~ (`div` z)

-- * Updates

updateSparseTexture :: MonadIO m => SparseTexture d px -> (StateT (SparseTexture d px) m ()) -> m (SparseTexture d px)
updateSparseTexture tex updateM = withTextureBound (tex^.sparseTexture) $ execStateT updateM tex

commitPage :: (MonadIO m, MonadState (SparseTexture3D px) m) => PageIndex -> Bool -> m ()
commitPage pageIndex@(V3 x y z) commit = do
  pages <- use pagesIn
  tex   <- use sparseTexture
  V3 pageSizeX pageSizeY pageSizeZ <- use pageSizes
  let V3 bw bh bd = tex^.textureDimension.whd

  when ( pages^.contains pageIndex /= commit ) $ do
    --io $ printf "commit: %s - %s : %s\n" (show $ tex^.textureObject) (show pageIndex) (show commit)
    --io $ printf "\tparam: %d; %d; %d\n" (x*pageSizeX) (y*pageSizeY) (z*pageSizeZ)
    --io $ printf "\tparam: %d; %d; %d\n" (pageSizeX) (pageSizeY) (pageSizeZ)
    glTexPageCommitmentARB (tex^.textureTarget) 0
      (fromIntegral $ x*pageSizeX) (fromIntegral $ y*pageSizeY) (fromIntegral $ z*pageSizeZ)
      (fromIntegral pageSizeX) (fromIntegral pageSizeY) (fromIntegral pageSizeZ)
      (if commit then GL_TRUE else GL_FALSE)

    -- currently just commit the complete mipmap chain
    -- technically we need to sample down to 1x1x1 but I made somewere a mistake, so GL complains
    -- about offset + width must be less or equal to texture width
    forM_ [1.. (tex^.textureLevel - 1)] $ \l -> do
      let base     = 2^l
          V3 w h d = fromIntegral <$> V3 bw bh bd & mapped %~ (`div` base)
          -- maxIdxs  = V3 (bw `div` pageSizeX) (bh `div` pageSizeY) (bd `div` pageSizeZ)
          mipIdx@(_, V3 mx my mz) = (fromIntegral l, pageIndex & mapped %~ (`div` base))
      mipmapClaims %= (if commit then MS.insert else MS.delete) mipIdx
      mips <- use mipmapClaims
      -- the base page possible claims or releases a mipmap page
      -- currenty we
      when (MS.occur mipIdx mips <= 1) $ do
        let off@(V3 offx offy offz) = V3 (mx*pageSizeX) (my*pageSizeY) (mz*pageSizeZ)
            V3 pw ph pd       = liftI2 min (V3 w h d) (off + V3 pageSizeX pageSizeY pageSizeZ) - off
        --io $ printf "\tmip %d: %d; %d; %d\n" l w h d
        --io $ printf "\t: %d; %d; %d\n" offx offy offz
        --io $ printf "\t: %d; %d; %d\n" pw ph pd
        glTexPageCommitmentARB (tex^.textureTarget) (fromIntegral l)
          (fromIntegral offx) (fromIntegral offy) (fromIntegral offz)
          (fromIntegral pw) (fromIntegral ph) (fromIntegral pd)
          (if commit then GL_TRUE else GL_FALSE)

  pagesIn.contains pageIndex .= commit

syncPages :: MonadIO m => SparseTexture3D pxs -> SparseTexture3D px -> m (SparseTexture3D px)
syncPages src dest = updateSparseTexture dest $ do
  forM_ newCommit $ (`commitPage` True)
  forM_ newDecommit $ (`commitPage` False)
 where
  newCommit = (src^.pagesIn) `difference` (dest^.pagesIn)
  newDecommit = (dest^.pagesIn) `difference` (src^.pagesIn)

instance IsRenderTarget (SparseTexture3D px) where
  getAttachments t = ([mkAttachment t], Nothing, Nothing)

instance FramebufferAttachment (SparseTexture3D px) where
  attach target s = attach target s . view sparseTexture
