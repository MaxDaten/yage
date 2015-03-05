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
module Yage.Rendering.Pipeline.Voxel.BaseVoxelize
  ( baseVoxelizePass
  , SparseVoxelBuffer
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
import           Linear
import           Data.Data
import qualified Data.Vector.Storable as VS hiding (forM_,find,findIndex,foldr1)
import qualified Data.Vector          as V
import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.StateVar
import           Quine.GL.Texture

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.BaseGPass

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- | Voxelization Modes
data VoxelizeMode =
    ProcessSceneVoxelization SparseVoxelBuffer
    -- ^ fine full resolution voxelization
  | ProcessPageMask PageMask3D
    -- ^ calculate page mask for sparse voxel texture
  deriving (Show,Ord,Eq,Generic)

-- | Rasterization Modes
data RasterizationMode =
    ConservativeHasselgren
  -- ^ [Hasselgren et. al 05] <https://developer.nvidia.com/gpugems/GPUGems2/gpugems2_chapter42.html>
  | ConservativeOtaku
  -- ^ otaku690 inspired <https://github.com/otaku690/SparseVoxelOctree>
  | Standard
    -- ^ no extra dilatation
  deriving (Show,Ord,Read,Data,Typeable,Eq,Generic,Enum)

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
  , g_RasterizationMode :: UniformVar RasterizationMode
  }

data FragmentShader = FragmentShader
  { albedoMaterial  :: UniformVar (Material MaterialColorAlpha (Texture2D PixelRGB8))
  , f_VoxelizeMode  :: UniformVar VoxelizeMode
  }

-- * Pass Resources

type SparseVoxelBuffer = SparseTexture3D PixelR32UI

data PassRes = PassRes
  { vao           :: VertexArray
  , voxelBuf      :: SparseVoxelBuffer
  -- , pageMaskPBO   :: Buffer (SVector PixelR8UI)
  , pageClearData :: SVector (PixelBaseComponent PixelR8UI)
  , pipe          :: Pipeline
  , vert          :: VertexShader
  , geom          :: GeometryShader
  , frag          :: FragmentShader
  }

type VoxelizePass m scene = Pass PassRes m scene (SparseVoxelBuffer, Box)


baseVoxelizePass :: (MonadIO m, MonadThrow m, HasBox scene, GBaseScene scene f ent i v) => Int -> Int -> Int -> YageResource (VoxelizePass m scene)
baseVoxelizePass width height depth = do
  res <- passRes
  return $ Pass res (runPass $ voxelBuf res)
 where
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

    voxBuf <- genSparseTexture3D width height depth
    let V3 w h d = voxBuf^.pageMask.textureDimension.whd
        pageMaskSize = w * h * d -- * components (pixelFormat (Proxy::Proxy PixelR8UI))
        pageClear = VS.replicate pageMaskSize (minBound :: Word8)

    --pbo <- createEmptyBuffer PixelPackBuffer StaticRead 2048

    return $ PassRes vao voxBuf pageClear pipeline vert geom frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader PassRes m, HasBox scene, GBaseScene scene f ent i v) => SparseVoxelBuffer -> RenderSystem m scene (SparseVoxelBuffer, Box)
  runPass v = flip mkStatefulRenderPass v $ \voxelBuf scene -> do
    PassRes{pageClearData,pipe,vert,geom,frag,vao} <- ask
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

    clearVoxelBuffer (voxelBuf^.sparseTexture) cleardata
    clearVoxelBuffer (voxelBuf^.pageMask) pageClearData

    -- memory layout
    setupGlobals geom frag (ProcessPageMask $ voxelBuf^.pageMask) (scene^.box)
    drawEntities vert geom frag (scene^.entities)

    glMemoryBarrier GL_SHADER_IMAGE_ACCESS_BARRIER_BIT

    -- map page mask
    -- and iterate over page mask and commit/decommit pages
    updatedBuffer <- updateSparseTexture voxelBuf $ do
      mask <- use pageMask
      tex  <- use sparseTexture
      withPixels3D mask $ \vec -> withTextureBound tex $ do
        let V3 pagesX pagesY _pagesZ = mask^.textureDimension.whd
        V.forM_ (V.indexed vec) $ \(i, p) -> do
          -- map the idx back to the page coord
          let pageId = V3 (i `mod` pagesX) (i `div` pagesX `mod` pagesY) (i `div` (pagesX * pagesY))
          commitPage pageId (p == (PixelR8UI maxBound))

    -- full resolution voxelize scene
    setupGlobals geom frag (ProcessSceneVoxelization $ updatedBuffer) (scene^.box)
    drawEntities vert geom frag (scene^.entities)

    glMemoryBarrier GL_SHADER_IMAGE_ACCESS_BARRIER_BIT

    return $! ((updatedBuffer, scene^.box), updatedBuffer)

  cleardata = VS.replicate (width * height * depth * componentCount (undefined :: PixelR32UI)) 0


-- | set all shader globals
setupGlobals :: MonadIO m => GeometryShader -> FragmentShader -> VoxelizeMode -> Box -> m ()
setupGlobals GeometryShader{..} FragmentShader{..} mode sceneBox = do
  x_Projection   $= xproj
  y_Projection   $= yproj
  z_Projection   $= zproj
  g_VoxelizeMode $= mode
  f_VoxelizeMode $= mode
  viewportIndexed X_AXIS $= xviewport
  viewportIndexed Y_AXIS $= yviewport
  viewportIndexed Z_AXIS $= zviewport
  -- Standard -- ConservativeOtaku -- ConservativeHasselgren
  g_RasterizationMode    $= ConservativeHasselgren

 where
  orthoDist = 1
  orthoX  = ortho (sceneBox^.lo._x) (sceneBox^.hi._x) (sceneBox^.lo._y) (sceneBox^.hi._y) orthoDist (orthoDist + sceneBox^.size._x)
  orthoY  = ortho (sceneBox^.lo._x) (sceneBox^.hi._x) (sceneBox^.lo._y) (sceneBox^.hi._y) orthoDist (orthoDist + sceneBox^.size._y)
  orthoZ  = ortho (sceneBox^.lo._x) (sceneBox^.hi._x) (sceneBox^.lo._y) (sceneBox^.hi._y) orthoDist (orthoDist + sceneBox^.size._z)
  xproj   = orthoX !*! lookAt (V3 (orthoDist + sceneBox^.hi._x) 0 0) (V3 0 0 0) (V3 0 1 0)
  yproj   = orthoY !*! lookAt (V3 0 (orthoDist + sceneBox^.hi._y) 0) (V3 0 0 0) (V3 0 0 (-1))
  zproj   = orthoZ !*! lookAt (V3 0 0 (orthoDist + sceneBox^.hi._z)) (V3 0 0 0) (V3 0 1 0)
  xviewport = fromIntegral <$> Rectangle 0 (V2 z y)
  yviewport = fromIntegral <$> Rectangle 0 (V2 x z)
  zviewport = fromIntegral <$> Rectangle 0 (V2 x y)
  V3 x y z = case mode of
    ProcessSceneVoxelization vbuff -> vbuff^.sparseTexture.textureDimension.whd
    ProcessPageMask mask  -> mask^.textureDimension.whd

drawEntities :: forall f ent i v m .
  (MonadIO m, MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v)
  => VertexShader
  -> GeometryShader
  -> FragmentShader
  -> (f ent)
  -> m ()
drawEntities VertexShader{..} GeometryShader{..} FragmentShader{..} ents = do

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
  <*> fmap (contramap (fromIntegral.fromEnum) . mkUniformVar.($=)) (programUniform programUniform1i prog "RasterizationMode")

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
      buffUniform $= Just (vbuff^.sparseTexture)
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
  io $ VS.unsafeWith cleardata $ glTexSubImage3D (tex^.textureTarget) 0
        0 0 0
        (fromIntegral w) (fromIntegral h) (fromIntegral d)
        (pixelFormat (Proxy :: Proxy px))
        (pixelType (Proxy :: Proxy px)) . castPtr
  boundTexture (tex^.textureTarget) 0 $= def
 where
  Tex3D w h d = tex^.textureDimension


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
