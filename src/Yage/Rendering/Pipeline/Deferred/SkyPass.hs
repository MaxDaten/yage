{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Yage.Rendering.Pipeline.Deferred.SkyPass
  ( SkyEntity
  , SkyVertex
  , SkyMaterial(..)
  , HasSkyMaterial(skyMaterial)
  , environmentMap
  , radianceMap
  , drawSky
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.GL
import           Yage.Math

import           Foreign.Ptr (nullPtr)
import           Yage.HDR
import           Yage.Material hiding (HasPosition, over, position)
import           Yage.Scene    hiding (Layout)
import           Yage.Uniform
import           Yage.Vertex   hiding (Texture)
import           Yage.Attribute
import           Yage.Viewport

import           Yage.Rendering.GL
import           Yage.Rendering.Pipeline.Deferred.Common
import           Yage.Rendering.RenderSystem
import           Yage.Rendering.Resources.GL

import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.Sampler
import           Quine.GL.VertexArray
import           Quine.StateVar
import           Quine.GL.ProgramPipeline

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Material
data SkyMaterial t = SkyMaterial
  { _skyMaterialEnvironmentMap :: Material MaterialColorAlpha (t PixelRGB8)
  , _skyMaterialRadianceMap    :: Material MaterialColorAlpha (t PixelRGB8)
  }

makeFields ''SkyMaterial
makeClassy ''SkyMaterial


-- * Vertex Attributes
type SkyVertex v = (HasPosition v Vec3)

type SkyEntity ent i v = (HasTransformation ent Double, HasRenderData ent i v, HasSkyMaterial ent TextureCube, SkyVertex v)

-- | Uniform StateVars of the fragment shader
data FragmentShader = FragmentShader
  { skyTexture     :: UniformVar (Material MaterialColorAlpha (TextureCube PixelRGB8))
  }

-- | Uniform StateVars of the fragment shader
data VertexShader = VertexShader
  { vPosition               :: VertexAttribute
  , skyTextureMatrix        :: UniformVar Mat4
  , viewMatrix              :: UniformVar Mat4
  , vpMatrix                :: UniformVar Mat4
  , modelMatrix             :: UniformVar Mat4
  }

-- data SkyPassInput =

drawSky :: (MonadReader w m, HasViewport w Int, MonadResource m) => SkyEntity sky i v => YageResource (RenderSystem m (sky, Camera, Texture2D px, Texture2D (DepthComponent32F Float)) (Texture2D px))
drawSky = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/sky.vert")
              , $(embedShaderFile "res/glsl/pass/sky.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  fbo <- glResource
  baseTexturesRef <- newIORef Nothing

  return $ mkStaticRenderPass $ \(sky, cam, albedo, depth) -> do
    boundFramebuffer RWFramebuffer $= fbo
    baseTexs <- get baseTexturesRef
    when (baseTexs /= Just (albedo^.textureObject,depth^.textureObject)) $ do
      void $ attachFramebuffer fbo [mkAttachment albedo] (Just $ mkAttachment depth) Nothing
      baseTexturesRef $= Just (albedo^.textureObject,depth^.textureObject)

    -- state setting
    glEnable GL_DEPTH_TEST
    glDepthFunc GL_LESS
    glDepthMask GL_TRUE

    glDisable GL_BLEND

    glFrontFace GL_CW
    glEnable GL_CULL_FACE
    glCullFace GL_BACK

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    setupSceneGlobals vert frag cam
    drawSkyEntity vert frag sky
    return albedo


setupSceneGlobals :: (MonadReader v m, HasViewport v Int, MonadIO m) => VertexShader -> FragmentShader -> Camera -> m ()
setupSceneGlobals VertexShader{..} FragmentShader{..} cam@Camera{..} = do
  mainViewport <- view viewport
  viewMatrix $= fmap realToFrac <$> (cam^.cameraMatrix)
  vpMatrix   $= fmap realToFrac <$> viewprojectionM mainViewport
 where
  viewprojectionM :: Viewport Int -> M44 Double
  viewprojectionM vp = projectionMatrix3D _cameraNearZ _cameraFarZ _cameraFovy (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)

drawSkyEntity :: forall sky i v m. (MonadIO m, SkyEntity sky i v) => VertexShader -> FragmentShader -> sky -> m ()
drawSkyEntity VertexShader{..} FragmentShader{..} ent = do
  modelMatrix       $= fmap realToFrac <$> (ent^.transformationMatrix)
  -- setup material
  skyTexture    $= ent^.skyMaterial.environmentMap
  -- bind vbo
  boundBufferAt ElementArrayBuffer $= ent^.indexBuffer
  boundBufferAt ArrayBuffer $= ent^.vertexBuffer

  -- update layout
  -- lastVertexLayout <- get vertexLayoutRef
  -- let currentLayout = gBaseVertexLayout (Proxy::Proxy v)
  -- when (lastVertexLayout /= Just currentLayout) $ do
  vPosition $= Just ((Proxy :: Proxy v)^.positionlayout)
  -- vertexLayoutRef $= Just currentLayout

  {-# SCC glDrawElements #-} throwWithStack $ glDrawElements (ent^.elementMode) (fromIntegral $ ent^.elementCount) (ent^.elementType) nullPtr


-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = do
  boundAttributeLocation prog "vPosition" $= VPOSITION
  VertexShader (setVertexAttribute VPOSITION)
    -- wraps the StateVar into a simple SettableStateVar
    <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "SkyTextureMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ViewMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "VPMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  cubeSampler <- mkCubeSampler
  FragmentShader <$> materialUniformRGBA prog cubeSampler "SkyTexture" "SkyColor"

-- * Sampler

mkCubeSampler :: YageResource (UniformSamplerCube px)
mkCubeSampler = throwWithStack $ samplerCube ENVIRONMENT_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_R $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR_MIPMAP_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_ARB_seamless_cubemap_per_texture $ do
    samplerParameteri sampler GL_TEXTURE_CUBE_MAP_SEAMLESS $= GL_TRUE
  return sampler
