{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Arrows              #-}
module Yage.Rendering.Pipeline.Voxel.Voxelize
  ( voxelizePass
  , VoxelBuffer
  , VoxelPageMask
  , VoxelizedScene(..), pageMask, voxelizedScene
  -- * VoxelizeMode for Visualization
  , VoxelizeMode(..)
  , voxelizeModeUniform
  ) where

import           Yage.Prelude

import           Yage.Lens
import           Yage.GL
import           Yage.Viewport
import           Yage.Vertex                             hiding (Texture)
import           Yage.Attribute
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout, componentCount)
import           Yage.Uniform                            as Uniform
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.Resources.GL.SparseTexture
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Linear
import           Data.Foldable (foldr1)
import           Data.List (findIndex,(!!))
import           Data.Vector.Storable as V hiding (forM_,find,findIndex,foldr1)
import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.GL.Sampler
import           Quine.StateVar
import           Quine.GL.Texture
import           Quine.GL.InternalFormat

-- import           Graphics.GL.Ext.ARB.ClearTexture

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.BaseGPass
import Graphics.GL.Ext.ARB.ViewportArray

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- | Voxelization Modes
data VoxelizeMode =
    ProcessSceneVoxelization VoxelBuffer
    -- ^ fine full resolution voxelization
  | ProcessPageMask VoxelPageMask
    -- ^ calculate page mask for sparse voxel texture
  deriving (Show,Ord,Eq,Generic)

-- * Shader

data VertexShader = VertexShader
  { vPosition               :: VertexAttribute
  , vTexture                :: VertexAttribute
  , vTangentX               :: VertexAttribute
  , vTangentZ               :: VertexAttribute
  , modelMatrix             :: UniformVar Mat4
  }

-- | Uniform StateVars of the fragment shader
data GeometryShader = GeometryShader
  { x_Projection   :: UniformVar Mat4
  , y_Projection   :: UniformVar Mat4
  , z_Projection   :: UniformVar Mat4
  , g_VoxelizeMode :: UniformVar VoxelizeMode
  }

data FragmentShader = FragmentShader
  { albedoMaterial  :: UniformVar (Material MaterialColorAlpha (Texture2D PixelRGB8))
  , f_VoxelizeMode  :: UniformVar VoxelizeMode
  }

type VoxelPageMask = Texture3D PixelR8UI
type VoxelBuffer = Texture3D PixelR32UI

data VoxelizedScene = VoxelizedScene
  { _voxelizedScene :: VoxelBuffer
  , _pageMask       :: VoxelPageMask
  } deriving (Show,Ord,Eq,Generic)

makeLenses ''VoxelizedScene

-- * Pass Resources

data PassRes = PassRes
  { vao           :: VertexArray
  , voxelBuffer   :: VoxelBuffer
  , pageMaskTex   :: VoxelPageMask
  , pageMaskPBO   :: Buffer (SVector PixelR8UI)
  , pageClearData :: SVector (PixelBaseComponent PixelR8UI)
  , pipe          :: Pipeline
  , vert          :: VertexShader
  , geom          :: GeometryShader
  , frag          :: FragmentShader
  }

type VoxelizePass m scene = Pass PassRes m scene VoxelizedScene


voxelizePass :: (MonadIO m, MonadThrow m, GBaseScene scene f ent i v) => Int -> Int -> Int -> YageResource (VoxelizePass m scene)
voxelizePass width height depth = Pass <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/voxel/voxelize.vert")
                , $(embedShaderFile "res/glsl/voxel/voxelize.frag")
                , $(embedShaderFile "res/glsl/voxel/voxelize.geom")]
                `compileShaderPipeline` includePaths

    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)
    Just geom <- traverse geometryUniforms  =<< get (geometryShader $ pipeline^.pipelineProgram)
    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)

    voxBuff <- genVoxelTexture width height depth
    pageMaskTex <- genPageMask voxBuff
    let V3 w h d = pageMaskTex^.textureDimension.whd
        pageMaskSize = w * h * d * 1 -- (components (pixelFormat (Proxy::Proxy PixelR8UI)))
        pageClear = V.replicate pageMaskSize (minBound :: Word8)

    pbo <- createEmptyBuffer PixelUnpackBuffer StaticRead pageMaskSize
    return $ PassRes vao voxBuff pageMaskTex pbo pageClear pipeline vert geom frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader PassRes m, GBaseScene scene f ent i v) => RenderSystem m scene VoxelizedScene
  runPass = mkStaticRenderPass $ \scene -> do
    PassRes{..} <- ask
    -- some state setting
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glDepthFunc GL_ALWAYS
    glDepthMask GL_FALSE
    glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    clearVoxelBuffer voxelBuffer cleardata
    clearVoxelBuffer pageMaskTex pageClearData

    -- memory layout
    setupGlobals geom frag (ProcessPageMask pageMaskTex)
    drawEntities vert geom frag (scene^.entities)

    -- map page Mask
    withMappedTexture pageMaskPBO GL_READ_ONLY pageMaskTex $ \vector -> do
      print $ V.length vector

    -- iterate over page mask and commit/decommit pages if voxelBuffer

    -- scene
    setupGlobals geom frag (ProcessSceneVoxelization voxelBuffer)
    drawEntities vert geom frag (scene^.entities)

    glMemoryBarrier GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
    return $ VoxelizedScene voxelBuffer pageMaskTex

  cleardata = V.replicate (width * height * depth * componentCount (undefined :: PixelR32UI)) 0


-- | set all shader globals
setupGlobals :: MonadIO m => GeometryShader -> FragmentShader -> VoxelizeMode -> m ()
setupGlobals GeometryShader{..} FragmentShader{..} mode = do
  x_Projection   $= xproj
  y_Projection   $= yproj
  z_Projection   $= zproj
  g_VoxelizeMode $= mode
  f_VoxelizeMode $= mode
  viewportIndexed X_AXIS $= xviewport
  viewportIndexed Y_AXIS $= yviewport
  viewportIndexed Z_AXIS $= zviewport
 where
  -- TODO: Scene extends
  orthoM  = ortho (-10) 10 (-10) 10 10 30
  xproj   = orthoM !*! lookAt (V3 20 0 0) (V3 0 0 0) (V3 0 1 0)
  yproj   = orthoM !*! lookAt (V3 0 20 0) (V3 0 0 0) (V3 0 0 (-1))
  zproj   = orthoM !*! lookAt (V3 0 0 20) (V3 0 0 0) (V3 0 1 0)
  xviewport = fromIntegral <$> Rectangle 0 (V2 z y)
  yviewport = fromIntegral <$> Rectangle 0 (V2 x z)
  zviewport = fromIntegral <$> Rectangle 0 (V2 x y)
  V3 x y z = case mode of
    ProcessSceneVoxelization vbuff -> vbuff^.textureDimension.whd
    ProcessPageMask mask  -> mask^.textureDimension.whd

drawEntities :: forall f ent i v m .
  (MonadIO m, MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v)
  => VertexShader
  -> GeometryShader
  -> FragmentShader
  -> (f ent)
  -> m ()
drawEntities VertexShader{..} GeometryShader{..} FragmentShader{..} ents =
  forM_ ents $ \ent -> do
    -- set entity globals
    modelMatrix       $= fmap realToFrac <$> (ent^.transformationMatrix)
    -- setup material
    albedoMaterial    $= ent^.gBaseMaterial.albedo

    -- bind vbo
    boundBufferAt ElementArrayBuffer $= ent^.indexBuffer
    boundBufferAt ArrayBuffer $= ent^.vertexBuffer

    vPosition $= Just ((Proxy :: Proxy v)^.positionlayout)
    vTexture  $= Just ((Proxy :: Proxy v)^.texturelayout)
    vTangentX $= Just ((Proxy :: Proxy v)^.tangentXlayout)
    vTangentZ $= Just ((Proxy :: Proxy v)^.tangentZlayout)

    {-# SCC glDrawElements #-} throwWithStack $ glDrawElements (ent^.elementMode) (fromIntegral $ ent^.elementCount) (ent^.elementType) nullPtr


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = do
  boundAttributeLocation prog "vPosition" $= VPOSITION
  boundAttributeLocation prog "vTexture"  $= VTEXTURE
  boundAttributeLocation prog "vTangentX" $= VTANGENTX
  boundAttributeLocation prog "vTangentZ" $= VTANGENTZ
  VertexShader (setVertexAttribute VPOSITION) (setVertexAttribute VTEXTURE) (setVertexAttribute VTANGENTX) (setVertexAttribute VTANGENTZ)
    -- wrap the StateVar into a simple SettableStateVar
    <$> fmap (mkUniformVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms prog = GeometryShader
  <$> fmap (mkUniformVar.($=)) (programUniform programUniformMatrix4f prog "X_Projection")
  <*> fmap (mkUniformVar.($=)) (programUniform programUniformMatrix4f prog "Y_Projection")
  <*> fmap (mkUniformVar.($=)) (programUniform programUniformMatrix4f prog "Z_Projection")
  <*> voxelizeModeUniform prog

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  albedoSampler    <- mkAlbedoSampler
  FragmentShader
    <$> materialUniformRGBA prog albedoSampler "AlbedoTexture" "AlbedoColor"
    <*> voxelizeModeUniform prog

voxelizeModeUniform :: MonadIO m => Program-> m (UniformVar VoxelizeMode)
voxelizeModeUniform prog = do
  buffUniform     <- imageTextureUniform (imageTexture3D 0 GL_WRITE_ONLY)
  maskUniform     <- imageTextureUniform (imageTexture3D 1 GL_READ_WRITE)
  flagMaskUniform <- programUniform programUniform1i prog "VoxelizeMode"
  return $ mkUniformVar $ \case
    ProcessSceneVoxelization vbuff -> do
      buffUniform $= Just vbuff
      maskUniform $= Nothing
      flagMaskUniform $= 0
    ProcessPageMask maskBuff -> do
      buffUniform $= Nothing
      maskUniform $= Just maskBuff
      flagMaskUniform $= 1

-- * Voxel Buffer

-- TODO Textures
clearVoxelBuffer :: forall m px. (MonadIO m, ImageFormat px, Pixel px) => Texture3D px -> SVector (PixelBaseComponent px) -> m ()
clearVoxelBuffer tex cleardata = do
  boundTexture (tex^.textureTarget) 0 $= tex^.textureObject
  io $ V.unsafeWith cleardata $ glTexSubImage3D (tex^.textureTarget) 0 0 0 0 (fromIntegral w) (fromIntegral h) (fromIntegral d) (pixelFormat (Proxy :: Proxy px)) (pixelType (Proxy :: Proxy px)) . castPtr
  boundTexture (tex^.textureTarget) 0 $= def
 where
  Tex3D w h d = tex^.textureDimension

genVoxelTexture :: forall px. (ImageFormat px, Pixel px) => Int -> Int -> Int -> YageResource (Texture3D px)
genVoxelTexture w h d = do
  tex <- createTexture3DWithSetup GL_TEXTURE_3D (Tex3D w h d) 1 $ \t -> do
    fmtIdx <- selectPageFormat t
    printf "Selected Page Format: %s\n" (show fmtIdx)
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_SPARSE_ARB $= GL_TRUE
    texParameteri GL_TEXTURE_3D GL_VIRTUAL_PAGE_SIZE_INDEX_ARB $= fst fmtIdx -- TODO on my machine 128x128x1
  --glTexPageCommitmentARB GL_TEXTURE_3D 0 0 0 0 (fromIntegral w) (fromIntegral h) (fromIntegral d) GL_FALSE
  glTexPageCommitmentARB GL_TEXTURE_3D 0 0 0 0 (fromIntegral w) (fromIntegral h) (fromIntegral d) GL_TRUE
  --clearVoxelBuffer tex cleardata
  return tex
 where
  selectPageFormat tex = do
    fmts <- virtualPageSizes3D tex
    case (findIndex  ((==) 1 . view _z)) fmts of
      Nothing     -> error "GL_TEXTURE_3D requires a tile depth == 1, no matching format found"
      Just fmtIdx -> return $ (fromIntegral fmtIdx, fmts!!fmtIdx)


genPageMask :: forall px. ImageFormat px => Texture3D px -> YageResource (VoxelPageMask)
genPageMask baseBuff = do
  pageSize <- virtualPageSize3D baseBuff
  io $ printf "PageSizes for %s: %s\n" (show baseBuff) (show pageSize)
  io $ printf "Max Sparse 3D Size: %d\n" maxSparseSize3D

  tex <- createTexture3DWithSetup GL_TEXTURE_3D (calcMaskSize (V3 8 8 8)) 1 $ \_ -> do
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
  io $ printf "created page mask: %s\n" (show tex)
  return tex
 where
  calcMaskSize (V3 x y z) = (baseBuff^.textureDimension)
    & whd._x %~ (`div` x)
    & whd._y %~ (`div` y)
    & whd._z %~ (`div` z)

-- * Sampler

mkAlbedoSampler :: YageResource (UniformSampler2D px)
mkAlbedoSampler = throwWithStack $ sampler2D ALBEDO_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
