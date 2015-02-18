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
module Yage.Rendering.Pipeline.Utils.Visualize3DTex
  ( visualize3DPass
  , mkVoxelVisTarget
  ) where

import           Yage.Prelude

import           Yage.Lens
import           Yage.GL
import           Yage.Viewport                           as GL
import           Yage.Camera
import           Yage.Vertex                             hiding (Texture)
import           Yage.Attribute
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout, componentCount)
import           Yage.Uniform                            as Uniform
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem
import           Yage.Rendering.RenderTarget
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

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Shader

data VertexShader = VertexShader
  { vpMatrix              :: UniformVar Mat4
  , modelMatrix           :: UniformVar Mat4
  , v_gridDim             :: UniformVar Vec2
  }

-- | Uniform StateVars of the fragment shader
data GeometryShader = GeometryShader
  { g_gridDim      :: UniformVar Vec2
  , toVis3D        :: UniformVar (Texture3D PixelR32UI)
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

data VoxelVisTarget = VoxelVisTarget
  { voxelVisScene :: Texture2D PixelRGBA8
  , voxelVisDepth :: Texture2D (DepthComponent24 Float)
  }

type Vis3DInput = (RenderTarget VoxelVisTarget, Texture3D PixelR32UI, Mat4, Camera)
type Vis3DOutput = Texture2D PixelRGBA8
type Vis3DPass m g = PassGEnv g PassRes m Vis3DInput Vis3DOutput

visualize3DPass :: (MonadIO m, MonadThrow m, HasViewport g Int) => YageResource (Vis3DPass m g)
visualize3DPass = PassGEnv <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/utils/vis3dTex.vert")
                , $(embedShaderFile "res/glsl/utils/vis3dTex.geom")
                , $(embedShaderFile "res/glsl/utils/vis3dTex.frag")]
                `compileShaderPipeline` includePaths

    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)
    Just geom <- traverse geometryUniforms  =<< get (geometryShader $ pipeline^.pipelineProgram)
    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)

    return $ PassRes vao pipeline vert geom frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader (PassEnv g PassRes) m, HasViewport g Int) => RenderSystem m Vis3DInput Vis3DOutput
  runPass = mkStaticRenderPass $ \(target, tex3d, modelM, cam) -> do
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
    glPointSize 20

    GL.glViewport $= target^.asRectangle
    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe
    let VertexShader{..}   = vert
        GeometryShader{..} = geom
        Tex3D w _ _ = tex3d^.textureDimension

    vpMatrix      $= fmap realToFrac <$> viewprojectionM cam mainViewport
    modelMatrix   $= modelM
    g_gridDim     $= V2 (fromIntegral w) (recip $ fromIntegral w)
    v_gridDim     $= V2 (fromIntegral w) (recip $ fromIntegral w)
    toVis3D       $= tex3d

    glDrawArrays GL_POINTS 0 (fromIntegral $ w * w * w )

    return $ target^.renderTarget.to voxelVisScene

  viewprojectionM :: Camera -> Viewport Int -> M44 Double
  viewprojectionM cam vp =
    projectionMatrix3D (cam^.cameraNearZ) (cam^.cameraFarZ) (cam^.cameraFovy) (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = VertexShader
  <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "VPMatrix")
  <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")
  <*> fmap (SettableStateVar.($=)) (programUniform programUniform2f prog "gridDim")

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms prog = do
  sampler <- mk3DSampler
  GeometryShader
    <$> fmap (SettableStateVar.($=)) (programUniform programUniform2f prog "gridDim")
    <*> fmap (contramap Just) (imageTextureUniform prog (imageTexture3D 0 GL_READ_ONLY) "VoxelAlbedo")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms _prog = return FragmentShader

-- * Sampler

mk3DSampler :: YageResource (UniformSampler3D PixelR32UI)
mk3DSampler = throwWithStack $ sampler3D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_NEAREST
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_NEAREST
  return s


mkVoxelVisTarget :: Rectangle Int -> YageResource VoxelVisTarget
mkVoxelVisTarget rect | V2 w h <- rect^.extend = VoxelVisTarget
  <$> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1
  <*> createTexture2D GL_TEXTURE_2D (Tex2D w h) 1

instance IsRenderTarget VoxelVisTarget where
  getAttachments (VoxelVisTarget c d) = ([mkAttachment c], Just $ mkAttachment d, Nothing)

instance Resizeable2D VoxelVisTarget where
  resize2D (VoxelVisTarget c d) w h = liftM2 VoxelVisTarget (resize2D c w h) (resize2D d w h)

instance GetRectangle VoxelVisTarget Int where
  asRectangle = to voxelVisScene.asRectangle
