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
  (
  ) where

import           Yage.Prelude

import           Yage.Math (m44_to_m33)
import           Yage.Lens
import           Yage.GL
import           Yage.Viewport                           as GL
import           Yage.Vertex                             hiding (Texture)
import           Yage.Attribute
import           Yage.Camera
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout)
import           Yage.Uniform                            as Uniform
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
import           Yage.Rendering.RenderSystem
import           Yage.Rendering.RenderTarget
import           Control.Monad.State.Strict (execStateT)
import           Foreign.Ptr
import           Linear

import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.GL.Sampler
import           Quine.StateVar

import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Shader

-- * Vertex Attributes
type Vertex v = (HasPosition v Vec3, HasTexture v Vec2, HasTangentX v Vec3, HasTangentZ v Vec4)

-- | Uniform StateVars of the fragment shader
data FragmentShader
data GeometryShader
data VertexShader

data VoxelBuffer
-- * Pass Resources

data PassRes = PassRes
  { vao          :: VertexArray
  , pipe         :: Pipeline
  , frag         :: FragmentShader
  , vert         :: VertexShader
  , geom         :: GeometryShader
  }

type VoxelizePass m globalEnv scene = PassGEnv globalEnv PassRes m (RenderTarget VoxelBuffer, scene, Camera) VoxelBuffer

voxelizePass :: (MonadIO m, MonadThrow m, HasViewport g Int) => YageResource (VoxelizePass m g scene)
voxelizePass = PassGEnv <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/pass/voxelize.vert")
                , $(embedShaderFile "res/glsl/pass/voxelize.frag")
                , $(embedShaderFile "res/glsl/pass/voxelize.geom")]
                `compileShaderPipeline` includePaths

    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)
    Just vert <- traverse vertexUniforms    =<< get (vertexShader $ pipeline^.pipelineProgram)
    Just geom <- traverse geometryUniforms  =<< get (geometryShader $ pipeline^.pipelineProgram)

    return $ PassRes vao pipeline frag vert geom

  runPass :: (MonadIO m, MonadThrow m, MonadReader (PassEnv g PassRes) m, HasViewport g Int) => RenderSystem m (RenderTarget VoxelBuffer, scene, Camera) VoxelBuffer
  runPass = mkStaticRenderPass $ \(target, scene, cam) -> do
    PassRes{..} <- view localEnv
    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)
    -- some state setting
    glEnable GL_DEPTH_TEST
    glDepthMask GL_TRUE
    glDepthFunc GL_LESS

    glDisable GL_BLEND
    -- glBlendEquation GL_FUNC_ADD
    -- glBlendFunc GL_ONE GL_ZERO

    glFrontFace GL_CCW
    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    return $ target^.renderTarget


    --vPosition $= Just ((Proxy :: Proxy v)^.positionlayout)
    --vTexture  $= Just ((Proxy :: Proxy v)^.texturelayout)
    --vTangentX $= Just ((Proxy :: Proxy v)^.tangentXlayout)
    --vTangentZ $= Just ((Proxy :: Proxy v)^.tangentZlayout)


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms = error "Voxelize.fragmentUniforms not implemented"

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms = error "Voxelize.fragmetUniforms not implemented"

geometryUniforms :: Program -> YageResource GeometryShader
geometryUniforms = error "Voxelize.geometryUniforms not implemented"

-- * Sampler

instance IsRenderTarget VoxelBuffer where
  getAttachments = error "Voxelize.getAttachments not implemented"

instance GetRectangle VoxelBuffer Int where
  asRectangle = error "Voxelize.asRectangle not implemented"

instance Resizeable2D VoxelBuffer where
  resize2D = error "Voxelize.geometryUniforms not implemented"
