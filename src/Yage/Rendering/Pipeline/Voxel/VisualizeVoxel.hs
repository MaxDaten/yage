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
  ( visualizeVoxelPass
  , mkVisVoxelTarget
  ) where

import           Yage.Prelude

import           Yage.Lens
import           Yage.GL
import           Yage.Viewport                           as GL
import           Yage.Camera
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout, componentCount)
import           Yage.Uniform                            as Uniform
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem
import           Yage.Rendering.RenderTarget
import           Linear
import           Quine.GL.Uniform
import           Quine.GL.Program
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.StateVar


import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Voxel.Voxelize

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Shader

data VertexShader = VertexShader
  { v_voxelizeMode     :: UniformVar VoxelizeMode
  }

-- | Uniform StateVars of the fragment shader
data GeometryShader = GeometryShader
  { g_voxelizeMode     :: UniformVar VoxelizeMode
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

type VisVoxelInput = (RenderTarget VisVoxelTarget, VoxelBuffer, VoxelPageMask, Mat4, Camera)
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
  runPass = mkStaticRenderPass $ \(target, voxelBuff, pageMask, modelM, cam) -> do
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

    -- setup globals shader vars
    let VertexShader{..}   = vert
        GeometryShader{..} = geom
        mode = VoxelPageMask pageMask
        V3 w h d = case mode of
          VoxelPageMask pageMask -> pageMask^.textureDimension.whd
          VoxelizeScene vbuff -> vbuff^.textureDimension.whd

    vpMatrix      $= fmap realToFrac <$> viewprojectionM cam mainViewport
    modelMatrix   $= modelM
    g_voxelizeMode  $= mode
    v_voxelizeMode  $= mode

    glDrawArrays GL_POINTS 0 (fromIntegral $ w * h * d )

    return $ target^.renderTarget.to voxelVisScene

  viewprojectionM :: Camera -> Viewport Int -> M44 Double
  viewprojectionM cam vp =
    projectionMatrix3D (cam^.cameraNearZ) (cam^.cameraFarZ) (cam^.cameraFovy) (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = VertexShader <$> voxelizeModeUniform prog

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms prog = GeometryShader
  <$> voxelizeModeUniform prog
  <*> fmap (contramap (fromIntegral.fromEnum) . toUniformVar) (programUniform programUniform1i prog "RenderEmpty")
  <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "VPMatrix")
  <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "ModelMatrix")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms _prog = return FragmentShader

-- * Sampler

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
