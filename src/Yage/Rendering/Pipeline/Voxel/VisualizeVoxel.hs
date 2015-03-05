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
module Yage.Rendering.Pipeline.Voxel.VisualizeVoxel
  ( VisualizeMode(..)
  , visualizeVoxelPass
  , mkVisVoxelTarget
  ) where

import Yage.Prelude

import Yage.Lens
import Yage.GL
import Yage.Viewport                           as GL
import Yage.Camera
import Yage.Material                           hiding (over, HasPosition, position)
import Yage.Scene                              hiding (Layout, componentCount)
import Yage.Uniform                            as Uniform
import Yage.Rendering.Resources.GL
import Yage.Rendering.Resources.GL.SparseTexture
import Yage.Rendering.GL
import Yage.Rendering.RenderSystem
import Yage.Rendering.RenderTarget
import Linear
import Data.Data
import Data.Foldable (foldr1)
import Quine.GL.Uniform
import Quine.GL.Program
import Quine.GL.VertexArray
import Quine.GL.ProgramPipeline
import Quine.StateVar
import Data.Time.Clock.POSIX


import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Voxel.BaseVoxelize
import Yage.Rendering.Pipeline.Voxel.UnpackVoxel (VoxelScene(..))

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

data VisualizeMode =
    VisualizeSceneVoxel
  | VisualizePageMask
  deriving (Ord,Eq,Read,Show,Data,Typeable,Generic,Enum)

-- * Shader

data VertexShader = VertexShader
  { v_voxelBaseBuf    :: UniformVar SparseVoxelBuffer
  , v_voxelTexture    :: UniformVar (Texture3D PixelRGBA8)
  , v_mode            :: UniformVar VisualizeMode
  , v_sampleLevel     :: UniformVar Int
  }

-- | Uniform StateVars of the fragment shader
data GeometryShader = GeometryShader
  { g_voxelBaseBuf     :: UniformVar SparseVoxelBuffer
  , g_mode             :: UniformVar VisualizeMode
  , g_voxelTexture     :: UniformVar (Texture3D PixelRGBA8)
  , g_sampleLevel      :: UniformVar Int
  , renderEmpty        :: UniformVar Bool
  , vpMatrix           :: UniformVar Mat4
  , modelMatrix        :: UniformVar Mat4
  }

data FragmentShader = FragmentShader

-- * Pass Resources

data PassRes = PassRes
  { vao          :: VertexArray
  , pipe         :: Pipeline
  , vert         :: VertexShader
  , geom         :: GeometryShader
  , frag         :: FragmentShader
  }

data VisVoxelTarget = VisVoxelTarget
  { voxelVisScene :: Texture2D PixelRGBA8
  , voxelVisDepth :: Texture2D (DepthComponent24 Float)
  }


type VisVoxelInput = (RenderTarget VisVoxelTarget, VoxelScene, Camera, [VisualizeMode])
type VisVoxelOutput = Texture2D PixelRGBA8
type VisVoxelPass m g = PassGEnv g PassRes m VisVoxelInput VisVoxelOutput

visualizeVoxelPass :: (MonadIO m, MonadThrow m, HasViewport g Int) => YageResource (VisVoxelPass m g)
visualizeVoxelPass = PassGEnv <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/voxel/visVoxelTex.vert")
                , $(embedShaderFile "res/glsl/voxel/visVoxelTex.geom")
                , $(embedShaderFile "res/glsl/voxel/visVoxelTex.frag")]
                `compileShaderPipeline` includePaths

    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)
    Just geom <- traverse geometryUniforms  =<< get (geometryShader $ pipeline^.pipelineProgram)
    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)

    return $ PassRes vao pipeline vert geom frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader (PassEnv g PassRes) m, HasViewport g Int) => RenderSystem m VisVoxelInput VisVoxelOutput
  runPass = mkStaticRenderPass $ \(target, VoxelScene unpackedTex bounds voxBuffer, cam, visModes) -> do
    PassRes{..}  <- view localEnv
    mainViewport <- view $ globalEnv.viewport

    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)
    -- some state setting
    glEnable GL_DEPTH_TEST
    glEnable GL_BLEND
    glBlendEquation GL_FUNC_ADD
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    glEnable GL_CULL_FACE
    glCullFace GL_BACK
    glDepthMask GL_TRUE
    glDepthFunc GL_LESS
    glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
    glPointSize 10

    GL.globalViewport $= target^.asRectangle
    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    time <- io $ round <$> getPOSIXTime
    -- setup globals shader vars
    let VertexShader{..}   = vert
        GeometryShader{..} = geom
        sampleLevel        = time `mod` (fromIntegral $ unpackedTex^.textureLevel)

    vpMatrix        $= fmap realToFrac <$> viewprojectionM cam mainViewport
    modelMatrix     $= (bounds & size //~ 4)^.transformationMatrix

    v_voxelBaseBuf  $= voxBuffer
    g_voxelBaseBuf  $= voxBuffer
    g_sampleLevel   $= sampleLevel
    v_sampleLevel   $= sampleLevel

    when (VisualizeSceneVoxel `oelem` visModes) $ do
      v_mode          $= VisualizeSceneVoxel
      g_mode          $= VisualizeSceneVoxel
      g_voxelTexture  $= unpackedTex
      v_voxelTexture  $= unpackedTex
      renderEmpty     $= False

      glDrawArrays GL_POINTS 0 (fromIntegral $ foldr1 (*) $ unpackedTex^.textureDimension.whd & mapped %~ (`div` (2^sampleLevel)) )

    when (VisualizePageMask `oelem` visModes) $ do
      v_mode          $= VisualizePageMask
      g_mode          $= VisualizePageMask
      renderEmpty     $= True

      glDrawArrays GL_POINTS 0 (fromIntegral $ foldr1 (*) $ voxBuffer^.pageMask.textureDimension.whd )

    return $ target^.renderTarget.to voxelVisScene

  viewprojectionM :: Camera -> Viewport Int -> M44 Double
  viewprojectionM cam vp =
    projectionMatrix3D (cam^.cameraNearZ) (cam^.cameraFarZ) (cam^.cameraFovy) (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)


-- * Shader Interfaces

vertexUniforms :: Program -> YageResource VertexShader
vertexUniforms prog = do
  smpl <- mkVoxelSampler 3
  VertexShader
    <$> baseVoxelSceneUniform prog
    <*> fmap (contramap Just) (samplerUniform prog smpl "VoxelRGB")
    <*> fmap (contramap (fromIntegral.fromEnum) . toUniformVar) (programUniform programUniform1i prog "VoxelizeMode")
    <*> fmap (contramap fromIntegral . toUniformVar) (programUniform programUniform1i prog "SampleLevel")

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms prog = do
  smpl <- mkVoxelSampler 3
  GeometryShader
    <$> baseVoxelSceneUniform prog
    <*> fmap (contramap (fromIntegral.fromEnum) . toUniformVar) (programUniform programUniform1i prog "VoxelizeMode")
    <*> fmap (contramap Just) (samplerUniform prog smpl "VoxelRGB")
    <*> fmap (contramap fromIntegral . toUniformVar) (programUniform programUniform1i prog "SampleLevel")
    <*> fmap (contramap (fromIntegral.fromEnum) . toUniformVar) (programUniform programUniform1i prog "RenderEmpty")
    <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "VPMatrix")
    <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "ModelMatrix")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms _prog = return FragmentShader


baseVoxelSceneUniform :: Program -> YageResource (UniformVar SparseVoxelBuffer)
baseVoxelSceneUniform prog = do
  vbuffSmpl    <- mkVoxelSampler 0
  maskSmpl     <- mkVoxelSampler 1
  vBuffUniform <- samplerUniform prog vbuffSmpl "VoxelBuffer"
  maskUniform  <- samplerUniform prog maskSmpl "VoxelPageMask"
  return $ mkUniformVar $ \sparse -> do
      vBuffUniform $= Just (sparse^.sparseTexture)
      maskUniform  $= Just (sparse^.pageMask)


-- * Sampler

mkVoxelSampler :: TextureUnit -> YageResource (UniformSampler3D px)
mkVoxelSampler unit = throwWithStack $ sampler3D unit <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_R $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  return s


mkVisVoxelTarget :: Rectangle Int -> YageResource VisVoxelTarget
mkVisVoxelTarget rect | V2 w h <- rect^.extend = VisVoxelTarget
  <$> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1
  <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1

instance IsRenderTarget VisVoxelTarget where
  getAttachments (VisVoxelTarget c d) = ([mkAttachment c], Just $ mkAttachment d, Nothing)

instance Resizeable2D VisVoxelTarget where
  resize2D (VisVoxelTarget c d) w h = liftM2 VisVoxelTarget (resize2D c w h) (resize2D d w h)

instance GetRectangle VisVoxelTarget Int where
  asRectangle = to voxelVisScene.asRectangle
