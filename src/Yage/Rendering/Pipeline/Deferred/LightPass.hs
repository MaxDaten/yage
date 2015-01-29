{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans -fno-warn-type-defaults #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TupleSections        #-}

module Yage.Rendering.Pipeline.Deferred.LightPass
  ( LightBuffer
  , drawLights
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
data FragmentShader = FragmentShader
  { radianceEnvironment  :: UniformVar (Maybe (TextureCube PixelRGB8))
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


type LightData = RenderData Word32 (V.Position Vec3)
type LightBuffer = Texture2D PixelRGBF11_11_10

drawLights :: (Foldable f, MonadResource m, MonadReader v m, HasViewport v Int) => YageResource (RenderSystem m (RenderTarget LightBuffer, f Light, (TextureCube PixelRGB8), Camera, GBuffer) LightBuffer)
drawLights = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/light.vert")
              , $(embedShaderFile "res/glsl/pass/light.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  [pointData, spotData, dirData] <- mapM fromMesh [pointMesh, spotMesh, dirMesh]
  let drawLights = drawLightEntities vert frag pointData spotData dirData

  return $ mkStaticRenderPass $ \(target, lights, radianceMap, cam, gBuffer) -> do
    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)

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

    setupSceneGlobals vert frag cam radianceMap gBuffer
    drawLights lights
    return $ target^.renderTarget


setupSceneGlobals :: (MonadReader v m, HasViewport v Int, MonadIO m) => VertexShader -> FragmentShader -> Camera -> TextureCube PixelRGB8 -> GBuffer -> m ()
setupSceneGlobals VertexShader{..} FragmentShader{..} cam@Camera{..} radiance gbuff = do
  vp <- view viewport
  let Rectangle xy0 xy1 = fromIntegral <$> vp^.rectangle

  viewToScreenMatrix  $= orthographicMatrix (xy0^._x) (xy1^._x) (xy1^._y) (xy0^._y) 0.0 1.0
  vpMatrix            $= fmap realToFrac <$> viewprojectionM vp
  viewMatrix          $= fmap realToFrac <$> (cam^.cameraMatrix)
  zProjectionRatio    $= zRatio
  radianceEnvironment $= Just radiance
  gBuffer             $= gbuff
  cameraPosition      $= realToFrac <$> cam^.position
 where
  viewprojectionM :: Viewport Int -> M44 Double
  viewprojectionM vp = projectionMatrix3D _cameraNearZ _cameraFarZ _cameraFovy (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)
  zRatio = realToFrac <$> V2 ((_cameraFarZ + _cameraNearZ) / (_cameraFarZ + _cameraNearZ)) (( 2.0 * _cameraNearZ * _cameraFarZ ) / ( _cameraFarZ - _cameraNearZ ))


-- | subject for instanced rendering
drawLightEntities :: (Foldable f, MonadIO m) => VertexShader -> FragmentShader -> LightData -> LightData -> LightData -> (f Light) -> m ()
drawLightEntities  VertexShader{..} FragmentShader{..} pointData spotData dirData lights = forM_ lights $ \light -> do
  let ldata = light^.lightType.to lightData

  -- set shader
  modelMatrix $= (fmap realToFrac <$> (light^.transformation.transformationMatrix))
  fragLight $= light
  vertLight $= light
  -- render data
  boundBufferAt ElementArrayBuffer $= ldata^.indexBuffer
  boundBufferAt ArrayBuffer $= ldata^.vertexBuffer
  vPosition $= Just ((Proxy :: Proxy (V.Position Vec3))^.V.positionlayout)

  {-# SCC glDrawElements #-} throwWithStack $ glDrawElements (ldata^.elementMode) (fromIntegral $ ldata^.elementCount) (ldata^.elementType) nullPtr
 where
  lightData :: LightType -> LightData
  lightData Pointlight{}       = pointData
  lightData Spotlight{}        = spotData
  lightData DirectionalLight{} = dirData

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
    aChannel  $= Just (gbuff^.aBuffer)
    bChannel  $= Just (gbuff^.bBuffer)
    cChannel  $= Just (gbuff^.cBuffer)
    depthTexture $= Just (gbuff^.depthBuffer)

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

mkCubeSampler :: YageResource (UniformSamplerCube PixelRGB8)
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
