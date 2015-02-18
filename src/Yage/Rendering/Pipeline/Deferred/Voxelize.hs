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
module Yage.Rendering.Pipeline.Deferred.Voxelize
  ( voxelizePass
  ) where

import           Yage.Prelude

import           Yage.Lens
import           Yage.GL
import           Yage.Viewport                           as GL
import           Yage.Vertex                             hiding (Texture)
import           Yage.Attribute
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout, componentCount)
import           Yage.Uniform                            as Uniform
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem
import           Foreign.Ptr
import           Linear
import           Data.Vector.Storable as V hiding (forM_)
import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.GL.Sampler
import           Quine.StateVar
import           Quine.GL.Texture

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Deferred.BaseGPass

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

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
  , gridDim        :: UniformVar Vec4
  }

data FragmentShader = FragmentShader
  { albedoMaterial  :: UniformVar (Material MaterialColorAlpha (Texture2D PixelRGB8))
  , voxelAlbedoImg  :: UniformVar (Texture3D PixelRGBA8)
  }

type VoxelBuffer = Texture3D PixelRGBA8
-- * Pass Resources

data PassRes = PassRes
  { vao          :: VertexArray
  , voxelBuffer  :: VoxelBuffer
  , pipe         :: Pipeline
  , vert         :: VertexShader
  , geom         :: GeometryShader
  , frag         :: FragmentShader
  }

type VoxelizePass m scene = Pass PassRes m scene VoxelBuffer


voxelizePass :: (MonadIO m, MonadThrow m, GBaseScene scene f ent i v) => Int -> Int -> Int -> YageResource (VoxelizePass m scene)
voxelizePass width height depth = Pass <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/pass/voxelize.vert")
                , $(embedShaderFile "res/glsl/pass/voxelize.frag")
                , $(embedShaderFile "res/glsl/pass/voxelize.geom")]
                `compileShaderPipeline` includePaths

    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)
    Just geom <- traverse geometryUniforms  =<< get (geometryShader $ pipeline^.pipelineProgram)
    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)


    voxBuff <- genVoxelTexture width height depth cleardata

    return $ PassRes vao voxBuff pipeline vert geom frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader PassRes m, GBaseScene scene f ent i v) => RenderSystem m scene VoxelBuffer
  runPass = mkStaticRenderPass $ \scene -> do
    PassRes{..} <- ask
    -- some state setting
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glDepthFunc GL_ALWAYS
    glDepthMask GL_FALSE
    glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE

    GL.glViewport $= Rectangle (V2 0 0) (V2 width height)

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    clearVoxelBuffer voxelBuffer cleardata

    setupGlobals geom frag voxelBuffer
    drawEntities vert geom frag (scene^.entities)

    glMemoryBarrier GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
    return $ voxelBuffer

  cleardata = V.replicate (width * height * depth * componentCount (undefined :: PixelRGBA8)) 0

setupGlobals :: MonadIO m => GeometryShader -> FragmentShader -> VoxelBuffer -> m ()
setupGlobals GeometryShader{..} FragmentShader{..} voxelBuffer = do
  gridDim        $= dim
  x_Projection   $= xproj
  y_Projection   $= yproj
  z_Projection   $= zproj
  voxelAlbedoImg $= voxelBuffer
 where
  Tex3D w h _ = voxelBuffer^.textureDimension
  dim     = V4 (fromIntegral w) (fromIntegral h) (recip $ fromIntegral w) (recip $ fromIntegral h)
  orthoM  = ortho (-10) 10 (-10) 10 10 30
  xproj   = orthoM !*! lookAt (V3 20 0 0) (V3 0 0 0) (V3 0 1 0)
  yproj   = orthoM !*! lookAt (V3 0 20 0) (V3 0 0 0) (V3 0 0 (-1))
  zproj   = orthoM !*! lookAt (V3 0 0 20) (V3 0 0 0) (V3 0 1 0)

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
    -- albedoMaterial    $= ent^.gBaseMaterial.albedo

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
    <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms prog = GeometryShader
  <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "X_Projection")
  <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "Y_Projection")
  <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "Z_Projection")
  <*> fmap (SettableStateVar.($=)) (programUniform programUniform4f prog "gridDim")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  albedoSampler    <- mkAlbedoSampler
  FragmentShader
    <$> materialUniformRGBA prog albedoSampler "AlbedoTexture" "AlbedoColor"
    <*> fmap (contramap Just) (imageTextureUniform prog (imageTexture3D 0 GL_READ_WRITE) "VoxelAlbedo")

-- * Voxel Buffer

-- TODO Textures
clearVoxelBuffer :: forall m px. (MonadIO m, ImageFormat px, Pixel px) => Texture3D px -> SVector (PixelBaseComponent px) -> m ()
clearVoxelBuffer tex cleardata = do
  boundTexture GL_TEXTURE_3D 0 $= tex^.textureObject
  io $ V.unsafeWith cleardata $ glTexSubImage3D GL_TEXTURE_3D 0 0 0 0 (fromIntegral w) (fromIntegral h) (fromIntegral d) (pixelFormat (Proxy :: Proxy px)) (pixelType (Proxy :: Proxy px)) . castPtr
  boundTexture GL_TEXTURE_3D 0 $= def
 where
  Tex3D w h d = tex^.textureDimension

genVoxelTexture :: forall px. (ImageFormat px, Pixel px) => Int -> Int -> Int -> SVector (PixelBaseComponent px) -> YageResource (Texture3D px)
genVoxelTexture w h d cleardata = do
  tex <- createTexture3D GL_TEXTURE_3D (Tex3D w h d) 1
  boundTexture GL_TEXTURE_3D 0 $= tex^.textureObject
  texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_NEAREST
  texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_NEAREST
  clearVoxelBuffer tex cleardata
  return tex

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
