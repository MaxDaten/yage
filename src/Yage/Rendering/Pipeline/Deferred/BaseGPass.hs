{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
-- | Renders all object parameters of a scene into the GBuffer.
module Yage.Rendering.Pipeline.Deferred.BaseGPass
  ( GBaseScene
  , GBaseEntity
  -- * Material
  , GBaseMaterial(..)
  , gBaseMaterialAlbedo
  , gBaseMaterialNormal
  , gBaseMaterialRoughness
  , gBaseMaterialMetallic
  -- * Render Data
  , GBaseVertexLayout(..)
  , HasGBaseVertexLayout(..)
  , GRenderData(..)
  -- * Pass Output
  , GBuffer(..)
  , drawGBuffers
  ) where

import           Yage                                    hiding (Layout)
import           Yage.Math (m44_to_m33)
import           Yage.Lens
import           Yage.GL
import           Yage.Camera
import           Yage.Material                           hiding (over)
import           Yage.Scene                              hiding (Layout)
import           Yage.Uniforms                           as Uniforms
import           Yage.HDR
import           Yage.Rendering.Resources.GL             hiding (vertexBuffer)
import           Yage.Rendering.GL

import           Data.Data
import           Data.Maybe
import           Foreign.Ptr

import           Quine.GL.Types
import           Quine.GL.Uniform
import           Quine.GL.VertexArray
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.ProgramPipeline

#include "definitions.h"
#include "textureUnits.h"
includePaths :: [FilePath]
includePaths = ["/res/glsl"]

-- * Material

data GBaseMaterial = GBaseMaterial
    { _gBaseMaterialAlbedo    :: Material MaterialColorAlpha (Texture PixelRGBA8)
    , _gBaseMaterialNormal    :: Material MaterialColorAlpha (Texture PixelRGBA8)
    , _gBaseMaterialRoughness :: Material Double (Texture Pixel8)
    , _gBaseMaterialMetallic  :: Material Double (Texture Pixel8)
    }

makeClassy ''GBaseMaterial
makeFields ''GBaseMaterial

-- * Render Data

data GBaseVertexLayout = GBaseVertexLayout
  { _vPosition :: !Layout
  , _vTexture  :: !Layout
  , _vTangentX :: !Layout
  , _vTangentZ :: !Layout
  } deriving (Show,Eq,Ord,Data,Typeable,Generic)

class HasGBaseVertexLayout t where
  gBaseVertexLayout :: p t -> GBaseVertexLayout

data GRenderData f v = GRenderData
  { _indexBuffer  :: Buffer (f Word32)
  , _vertexBuffer :: Buffer (f v)
  , _elementCount :: Int
  }

makeClassy ''GRenderData

-- * Shader

-- | Uniform StateVars of the fragment shader
data FragmentShader = FragmentShader
  { albedoMaterial     :: UniformVar (Material MaterialColorAlpha (Texture PixelRGBA8))
  , normalMaterial     :: UniformVar (Material MaterialColorAlpha (Texture PixelRGBA8))
  , roughnessMaterial  :: UniformVar (Material Double (Texture Pixel8))
  , metallicMaterial   :: UniformVar (Material Double (Texture Pixel8))
  }

type VertexAttribute = SettableStateVar (Maybe Layout)

-- | Uniform StateVars of the fragment shader
data VertexShader = VertexShader
  { vPosition               :: VertexAttribute
  , vTexture                :: VertexAttribute
  , vTangentX               :: VertexAttribute
  , vTangentZ               :: VertexAttribute
  , albedoTextureMatrix     :: UniformVar Mat4
  , normalTextureMatrix     :: UniformVar Mat4
  , roughnessTextureMatrix  :: UniformVar Mat4
  , metallicTextureMatrix   :: UniformVar Mat4
  , viewMatrix              :: UniformVar Mat4
  , vpMatrix                :: UniformVar Mat4
  , modelMatrix             :: UniformVar Mat4
  , normalMatrix            :: UniformVar Mat3
  }

-- * Scene

type GBaseScene ent env gui = Scene HDRCamera ent env gui

-- * Pass Output

-- | The output GBuffer of this pass
data GBuffer = GBuffer
  { _aBuffer     :: Texture PixelRGBA8
  , _bBuffer     :: Texture PixelRGBA8
  , _depthBuffer :: Texture (DepthComponent24 Float)
  } deriving (Typeable,Show,Generic)

-- * Draw To GBuffer

type GBaseEntity ent v f = (HasTransformation ent Double, HasGBaseMaterial ent, HasGRenderData ent f v, HasGBaseVertexLayout v)

drawGBuffers
  :: forall ent v f env gui. GBaseEntity ent v f
  => YageResource (RenderSystem (GBaseScene ent env gui, Viewport Int) GBuffer)
drawGBuffers = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/base.vert")
              , $(embedShaderFile "res/glsl/pass/base.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  -- setup vertex layout
  vPosition vert $= Just (_vPosition $ gBaseVertexLayout (Proxy::Proxy v))
  vTexture  vert $= Just (_vTexture  $ gBaseVertexLayout (Proxy::Proxy v))
  vTangentX vert $= Just (_vTangentX $ gBaseVertexLayout (Proxy::Proxy v))
  vTangentZ vert $= Just (_vTangentZ $ gBaseVertexLayout (Proxy::Proxy v))

  aChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  bChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  depthChannel <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture (DepthComponent24 Float)))
  fbo <- glResource

  lastViewportRef     <- newIORef (defaultViewport 1 1 :: Viewport Int)

  -- RenderPass
  return $ do
    (_scene, mainViewport) <- ask
    lastViewport <- get lastViewportRef

    -- resizing the framebuffer
    when (mainViewport /= lastViewport) $ do
      Yage.glViewport    $= mainViewport^.rectangle
      lastViewportRef    $= mainViewport
      let V2 w h = mainViewport^.rectangle.extend
      forM_ [aChannel, bChannel] $ \ch -> do
        modifyM ch $ \x -> resizeTexture2D x w h
      colors <- sequence [get aChannel, get bChannel]
      modifyM depthChannel $ \x -> resizeTexture2D x w h
      depth  <- get depthChannel
      void $ attachFramebuffer fbo (mkAttachment <$> colors) (Just $ mkAttachment depth) Nothing

    boundFramebuffer RWFramebuffer $= fbo

    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glEnable GL_DEPTH_TEST
    glDisable GL_BLEND

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    currentProgram $= def
    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    setupSceneGlobals vert frag
    drawScene vert frag

    GBuffer <$> get aChannel <*> get bChannel <*> get depthChannel

setupSceneGlobals :: (HasTransformation ent Double, HasGBaseMaterial ent) => VertexShader -> FragmentShader -> RenderSystem (GBaseScene ent env gui, Viewport Int) ()
setupSceneGlobals VertexShader{..} FragmentShader{..} = do
  (scene, mainViewport) <- ask
  viewMatrix $= fmap realToFrac <$> viewM scene
  vpMatrix   $= fmap realToFrac <$> viewprojectionM scene mainViewport
  return ()
 where
  viewM scene = scene^.camera.transformationMatrix
  viewprojectionM scene vp = projectionMatrix3D (scene^.camera.nearZ) (scene^.camera.farZ) (scene^.camera.fovy) (fromIntegral <$> vp^.rectangle) !*! viewM scene

drawScene :: (HasTransformation ent Double, HasGBaseMaterial ent, HasGRenderData ent f v) => VertexShader -> FragmentShader -> RenderSystem (GBaseScene ent env gui, Viewport Int) ()
drawScene VertexShader{..} FragmentShader{..} = do
  (scene, _mainViewport) <- ask
  forM_ (scene^.sceneEntities) $ \ent -> do
    -- set entity globals
    modelMatrix       $= fmap realToFrac <$> (ent^.transformationMatrix)
    normalMatrix      $= fmap realToFrac <$> (ent^.inverseTransformation.transformationMatrix.to m44_to_m33)
    -- setup material
    albedoMaterial    $= ent^.gBaseMaterial.albedo
    normalMaterial    $= ent^.gBaseMaterial.normal
    roughnessMaterial $= ent^.gBaseMaterial.roughness
    metallicMaterial  $= ent^.gBaseMaterial.metallic
    -- bind vbo
    boundBufferAt ArrayBuffer $= ent^.vertexBuffer
    boundBufferAt ElementArrayBuffer $= ent^.indexBuffer
    {-# SCC glDrawElements #-} throwWithStack $ glDrawElements GL_TRIANGLES (fromIntegral $ ent^.elementCount) GL_UNSIGNED_BYTE nullPtr

-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = VertexShader
  <$> (liftM (setVertexAttribute . fromJust) $ attributeLocation prog "vPosition")
  <*> (liftM (setVertexAttribute . fromJust) $ attributeLocation prog "vTexture")
  <*> (liftM (setVertexAttribute . fromJust) $ attributeLocation prog "vTangentX")
  <*> (liftM (setVertexAttribute . fromJust) $ attributeLocation prog "vTangentZ")
  <*> fmap setter (programUniform programUniformMatrix4f prog "AlbedoTextureMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "NormalTextureMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "RoughnessTextureMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "MetallicTextureMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "ViewMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "VPMatrix")
  <*> fmap setter (programUniform programUniformMatrix4f prog "ModelMatrix")
  <*> fmap setter (programUniform programUniformMatrix3f prog "NormalMatrix")
 where
  setter :: StateVar a -> SettableStateVar a
  setter (StateVar _ s) = SettableStateVar s

fragmentUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m FragmentShader
fragmentUniforms prog = FragmentShader
  <$> materialUniformColor prog ALBEDO_UNIT "AlbedoTexture" "AlbedoColor"
  <*> materialUniformColor prog NORMAL_UNIT "NormalTexture" "NormalColor"
  <*> materialUniformColor1 prog ROUGHNESS_UNIT "RoughnessTexture" "RoughnessIntensity"
  <*> materialUniformColor1 prog METALLIC_UNIT "MetallicTexture" "MetallicIntensity"

