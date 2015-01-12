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

-- | Renders all object parameters of a scene into the GBuffer.
module Yage.Rendering.Pipeline.Deferred.BaseGPass
  ( GBaseScene
  , GBaseEntity
  -- * Material
  , GBaseMaterial(..)
  , HasGBaseMaterial(..)
  , albedo
  , normalmap
  , roughness
  , metallic
  , defaultGBaseMaterial
  , gBaseMaterialRes
  -- * Vertex Attributes
  , GBaseVertex
  -- * Pass Output
  , GBuffer(..)
  , aBuffer
  , bBuffer
  , depthBuffer
  -- * Pass
  , drawGBuffers
  ) where

import           Yage                                    hiding (Layout, HasPosition, position)
import           Yage.Math (m44_to_m33)
import           Yage.Lens
import           Yage.GL
import           Yage.Vertex                             hiding (Texture)
import           Yage.Attribute
import           Yage.Camera
import           Yage.Material                           hiding (over, HasPosition, position)
import           Yage.Scene                              hiding (Layout)
import           Yage.Uniforms                           as Uniforms
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL
import           Foreign.Ptr

import           Quine.GL.Uniform
import           Quine.GL.Attribute
import           Quine.GL.Program
import           Quine.GL.Buffer
import           Quine.GL.VertexArray
import           Quine.GL.ProgramPipeline
import           Quine.GL.Sampler

import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"
#include "textureUnits.h"
#include "attributes.h"

-- * Material

data GBaseMaterial t = GBaseMaterial
  { _gBaseMaterialAlbedo    :: Material MaterialColorAlpha (t PixelRGB8)
  , _gBaseMaterialNormalmap :: Material MaterialColorAlpha (t PixelRGB8)
  , _gBaseMaterialRoughness :: Material Double (t Pixel8)
  , _gBaseMaterialMetallic  :: Material Double (t Pixel8)
  }

makeClassy ''GBaseMaterial
makeFields ''GBaseMaterial

-- * Shader

-- * Vertex Attributes
type GBaseVertex v = (HasPosition v Vec3, HasTexture v Vec2, HasTangentX v Vec3, HasTangentZ v Vec4)

-- | Uniform StateVars of the fragment shader
data FragmentShader = FragmentShader
  { albedoMaterial     :: UniformVar (Material MaterialColorAlpha (Texture PixelRGB8))
  , normalMaterial     :: UniformVar (Material MaterialColorAlpha (Texture PixelRGB8))
  , roughnessMaterial  :: UniformVar (Material Double (Texture Pixel8))
  , metallicMaterial   :: UniformVar (Material Double (Texture Pixel8))
  }


-- | Uniforms
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

-- type GBaseScene ent env gui = Scene HDRCamera ent env gui

-- * Pass Output

-- | The output GBuffer of this pass (for encoding see "res/glsl/pass/gbuffer.h")
data GBuffer = GBuffer
  { _aBuffer     :: Texture PixelRGBA8
  , _bBuffer     :: Texture PixelRGBA8
  , _depthBuffer :: Texture (DepthComponent24 Float)
  } deriving (Typeable,Show,Generic)

makeLenses ''GBuffer


type GBaseEntity ent i v = (HasTransformation ent Double, HasGBaseMaterial ent Texture, HasRenderData ent i v, GBaseVertex v)
type GBaseScene scene f ent i v = (MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v, HasEntities scene (f ent))

-- * Draw To GBuffer

drawGBuffers :: GBaseScene scene f ent i v => YageResource (RenderSystem (scene, Camera, Viewport Int) GBuffer)
drawGBuffers = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/base.vert")
              , $(embedShaderFile "res/glsl/pass/base.frag")]
              `compileShaderPipeline` includePaths


  Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

  aChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  bChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  depthChannel <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture (DepthComponent24 Float)))
  fbo <- glResource

  lastViewportRef     <- newIORef (defaultViewport 1 1 :: Viewport Int)

  -- RenderPass
  return $ do
    (scene, cam, mainViewport) <- ask
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
    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    setupSceneGlobals vert frag . pure (cam, mainViewport)
    drawEntities vert frag . pure (scene^.entities)

    GBuffer <$> get aChannel <*> get bChannel <*> get depthChannel

setupSceneGlobals :: VertexShader -> FragmentShader -> RenderSystem (Camera, Viewport Int) ()
setupSceneGlobals VertexShader{..} FragmentShader{..} = do
  (cam, mainViewport) <- ask
  viewMatrix $= fmap realToFrac <$> (cam^.cameraMatrix)
  vpMatrix   $= fmap realToFrac <$> viewprojectionM cam mainViewport
 where
  viewprojectionM :: Camera -> Viewport Int -> M44 Double
  viewprojectionM cam@Camera{..} vp = projectionMatrix3D _cameraNearZ _cameraFarZ _cameraFovy (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)

drawEntities :: forall f ent i v. (MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v)
  => VertexShader
  -> FragmentShader
  -> RenderSystem (f ent) ()
drawEntities VertexShader{..} FragmentShader{..} = do
  ents <- ask
  forM_ ents $ \ent -> do
    -- set entity globals
    modelMatrix       $= fmap realToFrac <$> (ent^.transformationMatrix)
    normalMatrix      $= fmap realToFrac <$> (ent^.inverseTransformation.transformationMatrix.to m44_to_m33)
    -- setup material
    albedoMaterial    $= ent^.gBaseMaterial.albedo
    normalMaterial    $= ent^.gBaseMaterial.normalmap
    roughnessMaterial $= ent^.gBaseMaterial.roughness
    metallicMaterial  $= ent^.gBaseMaterial.metallic

    -- bind vbo
    boundBufferAt ElementArrayBuffer $= ent^.indexBuffer
    boundBufferAt ArrayBuffer $= ent^.vertexBuffer

    -- update layout
    -- lastVertexLayout <- get vertexLayoutRef
    -- let currentLayout = gBaseVertexLayout (Proxy::Proxy v)
    -- when (lastVertexLayout /= Just currentLayout) $ do
    vPosition $= Just ((Proxy :: Proxy v)^.positionlayout)
    vTexture  $= Just ((Proxy :: Proxy v)^.texturelayout)
    vTangentX $= Just ((Proxy :: Proxy v)^.tangentXlayout)
    vTangentZ $= Just ((Proxy :: Proxy v)^.tangentZlayout)
    -- vertexLayoutRef $= Just currentLayout

    {-# SCC glDrawElements #-} throwWithStack $ glDrawElements (ent^.elementMode) (fromIntegral $ ent^.elementCount) (ent^.elementType) nullPtr

-- * Default Material

instance Default (GBaseMaterial Image) where
  def = defaultGBaseMaterial

defaultGBaseMaterial :: GBaseMaterial Image
defaultGBaseMaterial = GBaseMaterial
  { _gBaseMaterialAlbedo    = defaultMaterialSRGB
  , _gBaseMaterialNormalmap = defaultMaterialSRGB
  , _gBaseMaterialRoughness = mkMaterial 1.0 whiteDummy
  , _gBaseMaterialMetallic  = mkMaterial 1.0 blackDummy
  }

gBaseMaterialRes :: GBaseMaterial Image -> YageResource (GBaseMaterial Texture)
gBaseMaterialRes GBaseMaterial{..} = GBaseMaterial
  <$> materialRes _gBaseMaterialAlbedo
  <*> materialRes _gBaseMaterialNormalmap
  <*> materialRes _gBaseMaterialRoughness
  <*> materialRes _gBaseMaterialMetallic

-- * Shader Interfaces

vertexUniforms :: (MonadIO m, Functor m, Applicative m) => Program -> m VertexShader
vertexUniforms prog = do
  boundAttributeLocation prog "vPosition" $= VPOSITION
  boundAttributeLocation prog "vTexture"  $= VTEXTURE
  boundAttributeLocation prog "vTangentX" $= VTANGENTX
  boundAttributeLocation prog "vTangentZ" $= VTANGENTZ
  VertexShader (setVertexAttribute VPOSITION) (setVertexAttribute VTEXTURE) (setVertexAttribute VTANGENTX) (setVertexAttribute VTANGENTZ)
    -- wrap the StateVar into a simple SettableStateVar
    <$> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "AlbedoTextureMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "NormalTextureMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "RoughnessTextureMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "MetallicTextureMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ViewMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "VPMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix4f prog "ModelMatrix")
    <*> fmap (SettableStateVar.($=)) (programUniform programUniformMatrix3f prog "NormalMatrix")

fragmentUniforms :: Program -> YageResource FragmentShader
fragmentUniforms prog = do
  albedoSampler    <- mkAlbedoSampler
  normalSampler    <- mkNormalSampler
  roughnessSampler <- mkRoughnessSampler
  metallicSampler  <- mkMetallicSampler
  FragmentShader
    <$> materialUniformRGBA prog albedoSampler "AlbedoTexture" "AlbedoColor"
    <*> materialUniformRGBA prog normalSampler "NormalTexture" "NormalColor"
    <*> materialUniformIntensity prog roughnessSampler "RoughnessTexture" "RoughnessIntensity"
    <*> materialUniformIntensity prog metallicSampler "MetallicTexture" "MetallicIntensity"

-- * Sampler

mkAlbedoSampler :: YageResource (UniformSampler px)
mkAlbedoSampler = throwWithStack $ sampler2D ALBEDO_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

mkNormalSampler :: YageResource (UniformSampler px)
mkNormalSampler = throwWithStack $ sampler2D NORMAL_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

mkRoughnessSampler :: YageResource (UniformSampler px)
mkRoughnessSampler = throwWithStack $ sampler2D ROUGHNESS_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

mkMetallicSampler :: YageResource (UniformSampler px)
mkMetallicSampler = throwWithStack $ sampler2D METALLIC_UNIT <$> do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

