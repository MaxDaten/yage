{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Yage.Rendering.Pipeline.Deferred.LightPass
  ( drawLights
  ) where

import Yage.Prelude hiding (forM_)
import Yage.Lens
import Yage.Math hiding (lookAt)
import Yage.GL

import Data.Foldable
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
data FragmentShader = FragmentShader
  { radianceEnvironment  :: UniformVar (Texture PixelRGB8)
  , gBuffer              :: UniformVar GBuffer
  , cameraPosition       :: UniformVar Vec3
  , zProjectionRatio     :: UniformVar Vec2
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

data LightPassInput = LightPassInput
  {
  }

drawLights :: Foldable f => YageResource (RenderSystem (f Light, (Texture PixelRGB8), Camera, Viewport Int, GBuffer) (Texture PixelRGBF))
drawLights = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/light.vert")
              , $(embedShaderFile "res/glsl/pass/light.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  lBuffer <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBF))
  fbo <- glResource

  lastViewportRef     <- newIORef (defaultViewport 1 1 :: Viewport Int)
  let setupGlobals = setupSceneGlobals vert frag
  drawEntities <- drawLightEntities vert frag

  return $ do
    (lights, radianceMap, cam, mainViewport, gBuffer) <- ask
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
    glEnable GL_DEPTH_TEST
    glDepthMask GL_FALSE
    glDepthFunc GL_ALWAYS

    glDisable GL_BLEND
    -- glEnable GL_BLEND
    -- glBlendEquation GL_FUNC_ADD
    -- glBlendFunc GL_ONE GL_ONE

    glFrontFace GL_CCW
    glEnable GL_CULL_FACE
    glCullFace GL_FRONT

    glClearColor 0 0 0 1
    glClear GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    setupGlobals cam mainViewport radianceMap gBuffer
    drawEntities . pure lights
    get lBuffer

setupSceneGlobals :: VertexShader -> FragmentShader -> Camera -> Viewport Int -> Texture PixelRGB8 -> GBuffer -> RenderSystem a ()
setupSceneGlobals VertexShader{..} FragmentShader{..} cam@Camera{..} viewport radiance gbuff = do
  let Rectangle xy0 xy1 = fromIntegral <$> viewport^.rectangle

  viewToScreenMatrix  $= orthographicMatrix (xy0^._x) (xy1^._x) (xy1^._y) (xy0^._y) 0.0 1.0
  vpMatrix            $= fmap realToFrac <$> viewprojectionM
  viewMatrix          $= fmap realToFrac <$> (cam^.cameraMatrix)
  zProjectionRatio    $= zRatio
  radianceEnvironment $= radiance
  gBuffer             $= gbuff
  cameraPosition      $= realToFrac <$> cam^.position
 where
  viewprojectionM :: M44 Double
  viewprojectionM = projectionMatrix3D _cameraNearZ _cameraFarZ _cameraFovy (fromIntegral <$> viewport^.rectangle) !*! (cam^.cameraMatrix)
  zRatio = realToFrac <$> V2 ((_cameraFarZ + _cameraNearZ) / (_cameraFarZ + _cameraNearZ)) (( 2.0 * _cameraNearZ * _cameraFarZ ) / ( _cameraFarZ - _cameraNearZ ))

drawLightEntities :: Foldable f => VertexShader -> FragmentShader -> YageResource (RenderSystem (f Light) ())
drawLightEntities  VertexShader{..} FragmentShader{..} = do
  pointLightData       <- fromMesh pointMesh
  spotLightData        <- fromMesh spotMesh
  directionalLightData <- fromMesh dirMesh

  return $ do
    lights <- ask
    forM_ lights $ \light -> do
      -- set shader
      modelMatrix $= (fmap realToFrac <$> (light^.transformation.transformationMatrix))
      fragLight $= light
      vertLight $= light
      -- render data
      let rdata = case (light^.lightType) of
                    Pointlight{}       -> pointLightData
                    Spotlight{}        -> spotLightData
                    DirectionalLight{} -> directionalLightData
      boundBufferAt ElementArrayBuffer $= rdata^.indexBuffer
      boundBufferAt ArrayBuffer $= rdata^.vertexBuffer
      vPosition $= Just ((Proxy :: Proxy (V.Position Vec3))^.V.positionlayout)

      {-# SCC glDrawElements #-} throwWithStack $ glDrawElements (rdata^.elementMode) (fromIntegral $ rdata^.elementCount) (rdata^.elementType) nullPtr
 where
  pointMesh, spotMesh, dirMesh :: Mesh (V.Position Vec3)
  pointMesh = mkFromVerticesF "Pointligt" $ map V.Position . vertices . triangles $ geoSphere 2 1
  spotMesh  = mkFromVerticesF "Spotlight" $ map V.Position . vertices . triangles $ cone 1 1 24
  dirMesh   = mkFromVerticesF "DirectionalLight" $ V.Position <$> [0, 0, 0]

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
fragmentUniforms prog = do
  sampl <- mkCubeSampler
  FragmentShader
    <$> samplerUniform prog sampl "RadianceEnvironment"
    <*> gBufferUniform prog
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform3f prog "CameraPosition")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniform2f prog "ZProjRatio")
    <*> lightUniform prog "Light"

gBufferUniform :: Program -> YageResource (UniformVar GBuffer)
gBufferUniform prog = do
  gbufferSampler <- mkGBufferSampler
  aChannel <- samplerUniform prog (sampler2D G_CHANNEL_A gbufferSampler) "inChannelA"
  bChannel <- samplerUniform prog (sampler2D G_CHANNEL_B gbufferSampler) "inChannelB"
  cChannel <- samplerUniform prog (sampler2D G_CHANNEL_C gbufferSampler) "inChannelC"
  depthTexture <- samplerUniform prog (sampler2D G_DEPTH gbufferSampler) "DepthTexture"
  return $ SettableStateVar $ \gbuff -> do
    aChannel  $= gbuff^.aBuffer
    bChannel  $= gbuff^.bBuffer
    cChannel  $= gbuff^.cBuffer
    depthTexture $= gbuff^.depthBuffer

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

mkCubeSampler :: YageResource (UniformSampler PixelRGB8)
mkCubeSampler = throwWithStack $ samplerCube RADIANCE_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_ARB_seamless_cubemap_per_texture $ do
    samplerParameteri sampler GL_TEXTURE_CUBE_MAP_SEAMLESS $= GL_TRUE
  return sampler
