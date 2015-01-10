{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Yage.Rendering.Pipeline.Deferred.LightPass
  ( lightBuffer
  , drawLights
  ) where

import Yage.Prelude
import Yage.Lens
import Yage.Math
import Yage.GL


import Yage.Uniforms as U
import Yage.Camera
import Yage.Light
import Yage.Viewport as VP
import Yage.Scene
import Yage.Transformation
import Yage.Material
import Yage.Attribute

import Yage.Rendering.GL
import Yage.Rendering.Resources.GL
import Yage.Rendering.RenderSystem

import Yage.Rendering.Pipeline.Deferred.BaseGPass
import Yage.Rendering.Pipeline.Deferred.Common

import Quine.GL.Uniform
import Quine.GL.Attribute
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
data FragmentShader = FragmentShader
  { radianceEnvironment  :: UniformSampler
  , cameraPosition       :: UniformVar Vec3
  , fragLight            :: UniformVar Light
  }

-- | Uniform StateVars of the fragment shader
data VertexShader = VertexShader
  { vPosition            :: VertexAttribute
  , viewMatrix           :: UniformVar Mat4
  , vpMatrix             :: UniformVar Mat4
  , modelMatrix          :: UniformVar Mat4
  , viewToScreenMatrix   :: UniformVar Mat4
  , vertLight            :: UniformVar Light
  }

declareLenses [d|
  newtype LightBuffer = LightBuffer { lightBuffer :: (Texture PixelRGBA8) } deriving (Show,Generic)
  |]

drawLights :: YageResource (RenderSystem (f light, Camera, Viewport Int, GBuffer) LightBuffer)
drawLights = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/light.vert")
              , $(embedShaderFile "res/glsl/pass/light.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  lBuffer <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  fbo <- glResource

  lastViewportRef     <- newIORef (defaultViewport 1 1 :: Viewport Int)

  return $ do
    (lights, cam, mainViewport, gBuffer) <- ask
    lastViewport <- get lastViewportRef

    -- resizing the framebuffer
    when (mainViewport /= lastViewport) $ do
      VP.glViewport $= mainViewport^.rectangle
      lastViewportRef    $= mainViewport
      let V2 w h = mainViewport^.rectangle.extend
      modifyM lBuffer $ \t -> resizeTexture2D t w h
      buff <- get lBuffer
      void $ attachFramebuffer fbo [mkAttachment buff] (Just $ mkAttachment $ gBuffer^.depthBuffer) Nothing

    boundFramebuffer RWFramebuffer $= fbo

    -- some state setting
    -- we dont want to write to the depth buffer
    glDepthMask GL_FALSE
    glDepthFunc GL_LESS
    glEnable GL_DEPTH_TEST
    glDisable GL_BLEND
    glFrontFace GL_CCW
    glEnable GL_CULL_FACE
    glCullFace GL_FRONT
    glClear GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    -- setupSceneGlobals vert frag . pure (cam, mainViewport)
    -- drawSkyEntity vert frag . pure sky
    return $ LightBuffer (gBuffer^.aBuffer)


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = do
  boundAttributeLocation prog "vPosition" $= VPOSITION
  VertexShader (setVertexAttribute VPOSITION)
    <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ViewMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "VPMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ViewToScreenMatrix")
    <*> (lightUniform prog "Light")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = FragmentShader
  <$> mkCubeSampler
  <*> fmap (SettableStateVar.($=)) (programUniform programUniform3f prog "CameraPosition")
  <*> lightUniform prog "Light"

-- * Sampler

mkCubeSampler :: YageResource UniformSampler
mkCubeSampler = throwWithStack $ samplerCube ENVIRONMENT_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_ARB_seamless_cubemap_per_texture $ do
    traceM "XXX"
    samplerParameteri sampler GL_TEXTURE_CUBE_MAP_SEAMLESS $= GL_TRUE
  return sampler
