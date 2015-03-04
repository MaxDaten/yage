{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TupleSections        #-}

module Yage.Rendering.Pipeline.Deferred.PostAmbientPass
  ( PostAmbientBuffer
  , postAmbientPass
  ) where

import Yage.Prelude hiding (forM_)
import Yage.Lens
import Yage.Math hiding (lookAt)
import Yage.GL

import Yage.Uniform as U
import Yage.Camera
import Yage.Viewport as VP
import Yage.Scene
import Yage.Transformation
import Yage.Material

import Yage.Rendering.GL
import Yage.Rendering.Resources.GL
import Yage.Rendering.RenderSystem
import Yage.Rendering.RenderTarget

import Yage.Rendering.Pipeline.Deferred.BaseGPass
import Yage.Rendering.Pipeline.Voxel.Base
import Yage.Rendering.Pipeline.Deferred.Common

import Quine.GL.Uniform
import Quine.GL.Program
import Quine.GL.VertexArray
import Quine.StateVar
import Quine.GL.ProgramPipeline


#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"


-- | Uniform StateVars of the fragment shader
data FragmentShader px = FragmentShader
  { radianceEnvironment  :: UniformVar (Maybe (TextureCube px))
  , maxMipmapLevel       :: UniformVar MipmapLevel
  , diffuseMipmapOffset  :: UniformVar MipmapLevel
  , gBuffer              :: UniformVar GBuffer
  , cameraPos            :: UniformVar Vec3
  , zProjectionRatio     :: UniformVar Vec2
  , viewToWorld          :: UniformVar Mat4
  , sceneOpacityVoxel    :: UniformVar (Texture3D PixelRGBA8)
  , worldToVoxel         :: UniformVar Mat4
  }

data PassRes px = PassRes
  { vao         :: !VertexArray
  , pipe        :: !Pipeline
  , frag        :: !(FragmentShader px)
  }

type PostAmbientBuffer = Texture2D PixelRGB11_11_10F
type PostAmbientInput px = (RenderTarget PostAmbientBuffer, (TextureCube px), VoxelScene, Camera, GBuffer)
type PostAmbientPass m g px = PassGEnv g (PassRes px) m (PostAmbientInput px) PostAmbientBuffer

-- | Writes the ambient term additive to the given RenderTarget
postAmbientPass :: (MonadIO m, MonadThrow m, HasViewport g Int, ImageFormat px) => YageResource (PostAmbientPass m g px)
postAmbientPass = PassGEnv <$> passRes <*> pure runPass where
  passRes :: YageResource (PassRes px)
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
                , $(embedShaderFile "res/glsl/pass/ambient.frag")]
                `compileShaderPipeline` includePaths

    Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)

    return $ PassRes vao pipeline frag

  runPass :: (MonadIO m, MonadThrow m, MonadReader (PassEnv g (PassRes px)) m, HasViewport g Int, ImageFormat px)
          => RenderSystem m (PostAmbientInput px) PostAmbientBuffer
  runPass = mkStaticRenderPass $ \(target, radianceMap, VoxelScene voxTex bounds, cam, gBuf) -> do
    PassRes{..} <- view localEnv
    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)

    glDisable GL_DEPTH_TEST
    glDepthMask GL_FALSE
    glDepthFunc GL_ALWAYS

    glEnable GL_BLEND
    glBlendEquation GL_FUNC_ADD
    glBlendFunc GL_ONE GL_ONE

    glFrontFace GL_CCW
    glDisable GL_CULL_FACE

    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    -- Uniforms
    let FragmentShader{..} = frag
    zProjectionRatio    $= realToFrac <$> zRatio cam
    radianceEnvironment $= Just radianceMap
    maxMipmapLevel      $= radianceMap^.textureLevel
    diffuseMipmapOffset $= -2
    gBuffer             $= gBuf
    cameraPos           $= realToFrac <$> cam^.position
    viewToWorld         $= fmap realToFrac <$> (cam^.inverseCameraMatrix)
    sceneOpacityVoxel   $= voxTex
    worldToVoxel        $= traceShowId (bounds^.transformationMatrix)

    -- Draw
    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3
    return $ target^.renderTarget


fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  radSampl <- mkRadianceSampler
  opacitySampl <- mkOpacityVoxelSampler
  FragmentShader
    <$> samplerUniform prog radSampl "RadianceEnvironment"
    <*> fmap toUniformVar (programUniform programUniform1i prog "MaxMipmapLevel")
    <*> fmap toUniformVar (programUniform programUniform1i prog "DiffuseMipmapOffset")
    <*> gBufferUniform prog
    <*> fmap toUniformVar (programUniform programUniform3f prog "CameraPosition")
    <*> fmap toUniformVar (programUniform programUniform2f prog "ZProjRatio")
    <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "ViewToWorld")
    <*> fmap (contramap Just) (samplerUniform prog opacitySampl "SceneOpacityVoxel")
    <*> fmap toUniformVar (programUniform programUniformMatrix4f prog "WorldToVoxelSpace")

gBufferUniform :: Program -> YageResource (UniformVar GBuffer)
gBufferUniform prog = do
  gbufferSampler <- mkGBufferSampler
  _aChannel <- samplerUniform prog (sampler2D G_CHANNEL_A gbufferSampler) "inChannelA"
  _bChannel <- samplerUniform prog (sampler2D G_CHANNEL_B gbufferSampler) "inChannelB"
  _cChannel <- samplerUniform prog (sampler2D G_CHANNEL_C gbufferSampler) "inChannelC"
  _dChannel <- samplerUniform prog (sampler2D G_CHANNEL_D gbufferSampler) "inChannelD"
  depthTexture <- samplerUniform prog (sampler2D G_DEPTH gbufferSampler) "DepthTexture"
  return $ SettableStateVar $ \gbuff -> do
    _aChannel  $= Just (gbuff^.aChannel)
    _bChannel  $= Just (gbuff^.bChannel)
    _cChannel  $= Just (gbuff^.cChannel)
    _dChannel  $= Just (gbuff^.dChannel)
    depthTexture $= Just (gbuff^.depthChannel)

-- * Sampler

mkGBufferSampler :: YageResource Sampler
mkGBufferSampler = throwWithStack $ do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  -- when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

mkRadianceSampler :: YageResource (UniformSamplerCube px)
mkRadianceSampler = throwWithStack $ samplerCube RADIANCE_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR_MIPMAP_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_ARB_seamless_cubemap_per_texture $ do
    samplerParameteri sampler GL_TEXTURE_CUBE_MAP_SEAMLESS $= GL_TRUE
  return sampler

mkOpacityVoxelSampler :: YageResource (UniformSampler3D PixelRGBA8)
mkOpacityVoxelSampler = throwWithStack $ sampler3D OPACITY_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR_MIPMAP_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  return sampler
