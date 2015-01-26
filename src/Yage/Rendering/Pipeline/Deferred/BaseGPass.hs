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
  , cBuffer
  , depthBuffer
  -- * Pass
  , drawGBuffers
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
import           Foreign.Ptr
import           Linear
import           Control.Arrow

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
  , _cBuffer     :: Texture PixelRGBA8
  , _depthBuffer :: Texture (DepthComponent24 Float)
  } deriving (Typeable,Show,Generic)

makeLenses ''GBuffer


type GBaseEntity ent i v = (HasTransformation ent Double, HasGBaseMaterial ent Texture, HasRenderData ent i v, GBaseVertex v)
type GBaseScene scene f ent i v = (MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v, HasEntities scene (f ent))

-- * Draw To GBuffer

data PassRes = PassRes
  { vao          :: (ReleaseKey,VertexArray)
  , pipe         :: (ReleaseKey,Pipeline)
  , frag         :: (FragmentShader)
  , vert         :: (VertexShader)
  , fbo          :: (ReleaseKey,Framebuffer)
  , aChannel     :: Slot (Texture PixelRGBA8)
  , bChannel     :: Slot (Texture PixelRGBA8)
  , cChannel     :: Slot (Texture PixelRGBA8)
  , depthChannel :: Slot (Texture (DepthComponent24 Float))
  , lastViewport :: Viewport Int
  }

drawGBuffers :: (MonadReader w m, HasViewport w Int, MonadResource m, GBaseScene scene f ent i v) => RenderSystem m (scene, Camera) GBuffer
drawGBuffers = processResources >>> runPass  where
  processResources :: (MonadReader w m, HasViewport w Int, MonadResource m) => RenderSystem m s (PassRes,s)
  processResources = mkDynamicRenderPass $ \i -> do
    mainViewport <- view viewport
    let V2 w h = mainViewport^.rectangle.extend

    vao <- allocateAcquire glResource
    boundVertexArray $= snd vao

    pipeline <- allocateAcquire (
                [ $(embedShaderFile "res/glsl/pass/base.vert")
                , $(embedShaderFile "res/glsl/pass/base.frag")]
                `compileShaderPipeline` includePaths)


    Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^._2.pipelineProgram)
    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^._2.pipelineProgram)

    aChannel     <- snd <$> (allocateAcquire $ mkSlot $ createTexture2D GL_TEXTURE_2D w h)
    bChannel     <- snd <$> (allocateAcquire $ mkSlot $ createTexture2D GL_TEXTURE_2D w h)
    cChannel     <- snd <$> (allocateAcquire $ mkSlot $ createTexture2D GL_TEXTURE_2D w h)
    depthChannel <- snd <$> (allocateAcquire $ mkSlot $ createTexture2D GL_TEXTURE_2D w h)

    fbo <- allocateAcquire glResource
    colors <- mapM get [aChannel, bChannel, cChannel]
    depth <- get depthChannel
    void $ attachFramebuffer (snd fbo) (mkAttachment <$> colors) (Just $ mkAttachment depth) Nothing

    let res = PassRes vao pipeline frag vert fbo aChannel bChannel cChannel depthChannel mainViewport
    return ((res,i), maintainResources res)

  maintainResources :: (MonadReader w m, HasViewport w Int, MonadResource m) => PassRes -> RenderSystem m s (PassRes,s)
  maintainResources initRes = flip mkStatefulRenderPass initRes $ \res@PassRes{..} i -> do
    mainViewport <- view viewport
    -- resizing the framebuffer
    when (mainViewport /= lastViewport) $ do
      GL.glViewport    $= mainViewport^.rectangle
      let V2 w h = mainViewport^.rectangle.extend
      colors <- forM [aChannel, bChannel, cChannel] $ \ch -> do
        modifyM ch $ \x -> resizeTexture2D x w h
        get ch
      modifyM depthChannel $ \x -> resizeTexture2D x w h
      depth  <- get depthChannel
      void $ attachFramebuffer (snd fbo) (mkAttachment <$> colors) (Just $ mkAttachment depth) Nothing

    let newRes = res{lastViewport = mainViewport}
    return ((newRes,i),newRes)

  runPass = mkStaticRenderPass $ \(PassRes{..},(scene, cam)) -> do
    boundFramebuffer RWFramebuffer $= snd fbo

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
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= snd vao
    boundProgramPipeline $= pipe^._2.pipelineProgram
    checkPipelineError (snd pipe)

    setupSceneGlobals vert frag cam
    drawEntities vert frag (scene^.entities)

    GBuffer <$> get aChannel <*> get bChannel <*> get cChannel <*> get depthChannel


setupSceneGlobals :: (MonadReader v m, HasViewport v Int, MonadIO m) => VertexShader -> FragmentShader -> Camera -> m ()
setupSceneGlobals VertexShader{..} FragmentShader{..} cam@Camera{..} = do
  mainViewport <- view viewport
  viewMatrix $= fmap realToFrac <$> (cam^.cameraMatrix)
  vpMatrix   $= fmap realToFrac <$> viewprojectionM mainViewport
 where
  viewprojectionM :: Viewport Int -> M44 Double
  viewprojectionM vp = projectionMatrix3D _cameraNearZ _cameraFarZ _cameraFovy (fromIntegral <$> vp^.rectangle) !*! (cam^.cameraMatrix)

drawEntities :: forall f ent i v m .
  (MonadIO m, MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v)
  => VertexShader
  -> FragmentShader
  -> (f ent)
  -> m ()
drawEntities VertexShader{..} FragmentShader{..} ents = do
  forM_ ents $ \ent -> do
    -- set entity globals
    modelMatrix       $= fmap realToFrac <$> (ent^.transformationMatrix)
    normalMatrix      $= fmap realToFrac <$> (ent^.inverseTransformation.transformationMatrix.to m44_to_m33)
    -- setup material
    albedoMaterial          $= ent^.gBaseMaterial.albedo
    albedoTextureMatrix     $= fmap realToFrac <$> (ent^.gBaseMaterial.albedo.transformation.transformationMatrix)
    normalMaterial          $= ent^.gBaseMaterial.normalmap
    normalTextureMatrix     $= fmap realToFrac <$> (ent^.gBaseMaterial.normalmap.transformation.transformationMatrix)
    roughnessMaterial       $= ent^.gBaseMaterial.roughness
    roughnessTextureMatrix  $= fmap realToFrac <$> (ent^.gBaseMaterial.roughness.transformation.transformationMatrix)
    metallicMaterial        $= ent^.gBaseMaterial.metallic
    metallicTextureMatrix   $= fmap realToFrac <$> (ent^.gBaseMaterial.metallic.transformation.transformationMatrix)

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
  , _gBaseMaterialNormalmap = defaultMaterialSRGB & materialColor .~ rgb 4 4 4 `withOpacity` 0.75
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

fragmentUniforms :: MonadResource m => Program -> m FragmentShader
fragmentUniforms prog = do
  albedoSampler    <- snd <$> allocateAcquire mkAlbedoSampler
  normalSampler    <- snd <$> allocateAcquire mkNormalSampler
  roughnessSampler <- snd <$> allocateAcquire mkRoughnessSampler
  metallicSampler  <- snd <$> allocateAcquire mkMetallicSampler
  FragmentShader
    <$> materialUniformRGBA prog albedoSampler "AlbedoTexture" "AlbedoColor"
    <*> materialUniformRGBA prog normalSampler "NormalTexture" "NormalColor"
    <*> materialUniformIntensity prog roughnessSampler "RoughnessTexture" "RoughnessIntensity"
    <*> materialUniformIntensity prog metallicSampler "MetallicTexture" "MetallicIntensity"

-- * Sampler

mkAlbedoSampler :: YageResource (UniformSampler px)
mkAlbedoSampler = throwWithStack $ sampler2D ALBEDO_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkNormalSampler :: YageResource (UniformSampler px)
mkNormalSampler = throwWithStack $ sampler2D NORMAL_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkRoughnessSampler :: YageResource (UniformSampler px)
mkRoughnessSampler = throwWithStack $ sampler2D ROUGHNESS_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkMetallicSampler :: YageResource (UniformSampler px)
mkMetallicSampler = throwWithStack $ sampler2D METALLIC_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

