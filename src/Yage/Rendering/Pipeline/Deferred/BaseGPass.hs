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
  , aChannel
  , bChannel
  , cChannel
  , dChannel
  , depthChannel
  -- * Pass
  , gPass
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
import           Yage.Rendering.RenderTarget
import           Control.Monad.State.Strict (execStateT)
import           Foreign.Ptr
import           Linear

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
  { albedoMaterial     :: UniformVar (Material MaterialColorAlpha (Texture2D PixelRGB8))
  , normalMaterial     :: UniformVar (Material MaterialColorAlpha (Texture2D PixelRGB8))
  , roughnessMaterial  :: UniformVar (Material Double (Texture2D Pixel8))
  , metallicMaterial   :: UniformVar (Material Double (Texture2D Pixel8))
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
  { _aChannel     :: Texture2D PixelRGBA8
  , _bChannel     :: Texture2D PixelRGBA8
  , _cChannel     :: Texture2D PixelRGB32F
  , _dChannel     :: Texture2D PixelRGB32F
  , _depthChannel :: Texture2D (DepthComponent32F Float)
  } deriving (Typeable,Show,Generic)

makeLenses ''GBuffer


type GBaseEntity ent i v = (HasTransformation ent Double, HasGBaseMaterial ent Texture2D, HasRenderData ent i v, GBaseVertex v)
type GBaseScene scene f ent i v = (MonoFoldable (f ent), GBaseEntity (Element (f ent)) i v, HasEntities scene (f ent))

-- * Draw To GBuffer

data PassRes = PassRes
  { vao          :: VertexArray
  , pipe         :: Pipeline
  , frag         :: FragmentShader
  , vert         :: VertexShader
  }

type BaseGPass m globalEnv scene = PassGEnv globalEnv PassRes m (RenderTarget GBuffer, scene, Camera) GBuffer

gPass :: (MonadIO m, MonadThrow m, HasViewport g Int, GBaseScene scene f ent i v) => YageResource (BaseGPass m g scene)
gPass = PassGEnv <$> passRes <*> pure runPass where
  passRes :: YageResource PassRes
  passRes = do
    vao <- glResource
    boundVertexArray $= vao

    pipeline <- [ $(embedShaderFile "res/glsl/pass/base.vert")
                , $(embedShaderFile "res/glsl/pass/base.frag")]
                `compileShaderPipeline` includePaths

    Just frag <- traverse fragmentUniforms =<< get (fragmentShader $ pipeline^.pipelineProgram)
    Just vert <- traverse vertexUniforms =<< get (vertexShader $ pipeline^.pipelineProgram)

    return $ PassRes vao pipeline frag vert

  runPass :: (MonadIO m, MonadThrow m, MonadReader (PassEnv g PassRes) m, HasViewport g Int, GBaseScene scene f ent i v) => RenderSystem m (RenderTarget GBuffer, scene, Camera) GBuffer
  runPass = mkStaticRenderPass $ \(target, scene, cam) -> do
    PassRes{..} <- view localEnv
    boundFramebuffer RWFramebuffer $= (target^.framebufferObj)
    -- some state setting
    glEnable GL_DEPTH_TEST
    glDisable GL_BLEND
    glEnable GL_CULL_FACE
    glDepthMask GL_TRUE
    glDepthFunc GL_LESS
    glFrontFace GL_CCW
    glCullFace GL_BACK

    GL.globalViewport $= target^.asRectangle
    glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    -- set globals
    {-# SCC boundVertexArray #-} throwWithStack $ boundVertexArray $= vao
    boundProgramPipeline $= pipe^.pipelineProgram
    checkPipelineError pipe

    setupSceneGlobals vert frag cam
    drawEntities vert frag (scene^.entities)

    return $ target^.renderTarget


setupSceneGlobals :: (MonadReader (PassEnv g l) m, HasViewport g Int, MonadIO m) => VertexShader -> FragmentShader -> Camera -> m ()
setupSceneGlobals VertexShader{..} FragmentShader{..} cam@Camera{..} = do
  mainViewport <- view $ globalEnv.viewport
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
drawEntities VertexShader{..} FragmentShader{..} ents =
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
      & materialColor   .~ rgb 2 2 2 `withOpacity` 0.75
      & materialTexture .~ zNormalDummy
  , _gBaseMaterialRoughness = mkMaterial 1.0 whiteDummy
  , _gBaseMaterialMetallic  = mkMaterial 0.0 whiteDummy
  }

gBaseMaterialRes :: GBaseMaterial Image -> YageResource (GBaseMaterial Texture2D)
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

mkAlbedoSampler :: YageResource (UniformSampler2D px)
mkAlbedoSampler = throwWithStack $ sampler2D ALBEDO_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkNormalSampler :: YageResource (UniformSampler2D px)
mkNormalSampler = throwWithStack $ sampler2D NORMAL_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkRoughnessSampler :: YageResource (UniformSampler2D px)
mkRoughnessSampler = throwWithStack $ sampler2D ROUGHNESS_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

mkMetallicSampler :: YageResource (UniformSampler2D px)
mkMetallicSampler = throwWithStack $ sampler2D METALLIC_UNIT <$> do
  s <- glResource
  samplerParameteri s GL_TEXTURE_WRAP_S $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_WRAP_T $= GL_REPEAT
  samplerParameteri s GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri s GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $ samplerParameterf s GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return s

instance IsRenderTarget GBuffer where
  getAttachments GBuffer{..} =
    ( [ mkAttachment _aChannel
      , mkAttachment _bChannel
      , mkAttachment _cChannel
      , mkAttachment _dChannel
      ]
    , Just $ mkAttachment _depthChannel, Nothing)

instance GetRectangle GBuffer Int where
  asRectangle = aChannel.asRectangle

instance Resizeable2D GBuffer where
  resize2D gbuff w h = flip execStateT gbuff $ do
    aChannel <~ resize2D (gbuff^.aChannel) w h
    bChannel <~ resize2D (gbuff^.bChannel) w h
    cChannel <~ resize2D (gbuff^.cChannel) w h
    dChannel <~ resize2D (gbuff^.dChannel) w h
    depthChannel <~ resize2D (gbuff^.depthChannel) w h
