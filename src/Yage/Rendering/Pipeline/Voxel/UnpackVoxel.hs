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
module Yage.Rendering.Pipeline.Voxel.UnpackVoxel
  ( unpackVoxelPass
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
import           Yage.Rendering.RenderTarget
import           Foreign.Ptr
import           Foreign.Marshal.Array
import           Linear
import           GHC.Real (lcm)
import           Data.Data
import           Data.Foldable (foldr1)
import           Data.List (findIndex,(!!))
import qualified Data.Vector.Storable as VS hiding (forM_,find,findIndex,foldr1)
import qualified Data.Vector          as V
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
import           Quine.Geometry.Box

-- import           Graphics.GL.Ext.ARB.ClearTexture

import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.Pipeline.Voxel.Voxelize

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Shader

data FragmentShader = FragmentShader
  { sampleTexture  :: UniformVar (Texture3D PixelR32UI)
  }

-- * Pass Resources

data PassRes = PassRes
  { vao           :: VertexArray
  , pipe          :: Pipeline
  , frag          :: FragmentShader
  , target        :: RenderTarget (Texture3D PixelRGB8)
  }

type UnpackVoxelPass m = Pass PassRes m VoxelizedScene (Texture3D PixelRGB8)

-- | Takes the unsigned integer voxel texture and converts it into a better usable RGB channel texture
unpackVoxelPass :: (MonadIO m, MonadThrow m) => Int -> Int -> Int -> YageResource (UnpackVoxelPass m)
unpackVoxelPass width height depth = Pass <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
                , $(embedShaderFile "res/glsl/voxel/unpackVoxel.frag")
                , $(embedShaderFile "res/glsl/voxel/unpackVoxel.geom")
                ]
                `compileShaderPipeline` includePaths

    Just frag <- traverse fragmentUniforms  =<< get (fragmentShader $ pipeline^.pipelineProgram)

    voxTarget <- mkRenderTarget =<< createTargetTexture

    return $ PassRes vao pipeline frag voxTarget

  runPass :: (MonadIO m, MonadThrow m, MonadReader PassRes m) => RenderSystem m VoxelizedScene (Texture3D PixelRGB8)
  runPass = mkStaticRenderPass $ \inTex -> do
    PassRes{..} <- ask
    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)
    -- some state setting
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glDepthFunc GL_ALWAYS
    glDepthMask GL_FALSE

    globalViewport $= Rectangle 0 (V2 width height)
    glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
    glClearColor 0 0 1 1
    glClear GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    sampleTexture frag $= inTex^.voxelizedScene
    throwWithStack $ glDrawArraysInstanced GL_TRIANGLES 0 3 (fromIntegral $ target^.renderTarget.textureDimension.whd._z)

    withTextureBound (target^.renderTarget) $ glGenerateMipmap GL_TEXTURE_3D
    return $ target^.renderTarget

  createTargetTexture = createTexture3DWithSetup GL_TEXTURE_3D (Tex3D width height depth) (traceShowId $ truncate $ traceShowId $ logBase 2 $ fromIntegral width) $ \t -> do
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
    texParameteri GL_TEXTURE_3D GL_TEXTURE_BASE_LEVEL $= 0
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAX_LEVEL  $= (truncate $ logBase 2 $ fromIntegral width)
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MIN_FILTER $= GL_LINEAR
    texParameteri GL_TEXTURE_3D GL_TEXTURE_MAG_FILTER $= GL_LINEAR
    texParameteri GL_TEXTURE_3D GL_TEXTURE_SPARSE_ARB $= GL_FALSE
-- * Shader Interfaces

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  smpl <- mkSampler
  FragmentShader
    <$> fmap (contramap Just) (samplerUniform prog smpl "iTexture")


mkSampler :: YageResource (UniformSampler3D px)
mkSampler = throwWithStack $ sampler3D 0 <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s
