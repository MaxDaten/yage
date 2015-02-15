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

import Data.Foldable (forM_)
import Foreign.Ptr (nullPtr)

import Yage.Uniform as U
import Yage.Camera
import Yage.Light
import Yage.Viewport as VP
import Yage.Scene
import Yage.Transformation
import Yage.Material
import qualified Yage.Vertex as V
import Yage.Attribute
import Yage.Geometry3D

import Yage.Rendering.GL
import Yage.Rendering.Resources.GL
import Yage.Rendering.RenderSystem
import Yage.Rendering.RenderTarget

import Yage.Rendering.Pipeline.Deferred.BaseGPass
import Yage.Rendering.Pipeline.Deferred.Common

import Quine.GL.Uniform
import Quine.GL.Attribute hiding (normalize)
import Quine.GL.Program
import Quine.GL.Buffer
import Quine.GL.Sampler
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
  , cameraPosition       :: UniformVar Vec3
  , zProjectionRatio     :: UniformVar Vec2
  , viewToWorld          :: UniformVar Mat4
  }

data PassRes px = PassRes
  { vao         :: !VertexArray
  , pipe        :: !Pipeline
  , frag        :: !(FragmentShader px)
  }

type PostAmbientBuffer = Texture2D PixelRGB11_11_10F
type PostAmbientInput px = (RenderTarget PostAmbientBuffer, (TextureCube px), Camera, GBuffer)
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
  runPass = mkStaticRenderPass $ \(target, radianceMap, cam, gBuffer) -> do
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

    setupSceneGlobals frag cam radianceMap gBuffer
    throwWithStack $ glDrawArrays GL_TRIANGLES 0 3
    return $ target^.renderTarget


setupSceneGlobals :: (MonadReader (PassEnv g l) m, HasViewport g Int, MonadIO m) => FragmentShader px -> Camera -> TextureCube px -> GBuffer -> m ()
setupSceneGlobals FragmentShader{..} cam@Camera{..} radiance gbuff = do
  zProjectionRatio    $= zRatio
  radianceEnvironment $= Just radiance
  maxMipmapLevel      $= radiance^.textureLevel
  diffuseMipmapOffset $= -2
  gBuffer             $= gbuff
  cameraPosition      $= realToFrac <$> cam^.position
  viewToWorld         $= fmap realToFrac <$> (cam^.inverseCameraMatrix)
 where
  zRatio = realToFrac <$> V2 ((_cameraFarZ + _cameraNearZ) / (_cameraFarZ + _cameraNearZ)) (( 2.0 * _cameraNearZ * _cameraFarZ ) / ( _cameraFarZ - _cameraNearZ ))

fragmentUniforms :: Program -> YageResource (FragmentShader px)
fragmentUniforms prog = do
  sampl <- mkRadianceSampler
  FragmentShader
    <$> samplerUniform prog sampl "RadianceEnvironment"
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform1i prog "MaxMipmapLevel")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform1i prog "DiffuseMipmapOffset")
    <*> gBufferUniform prog
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform3f prog "CameraPosition")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform2f prog "ZProjRatio")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ViewToWorld")

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
