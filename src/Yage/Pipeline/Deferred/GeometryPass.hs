{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
module Yage.Pipeline.Deferred.GeometryPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Uniforms as Uniforms
import Yage.Camera
import Yage.Scene
import Yage.Material

import Yage.Rendering
import Yage.Rendering.Textures              (mkTextureSpec')

import qualified Graphics.Rendering.OpenGL as GL


--type GeoGlobalUniforms = PerspectiveUniforms ++ [YZFarPlane, YAlbedoTex, YNormalTex]
type AlbedoMaterialU   = YMaterial "AlbedoColor" "AlbedoTexture"
type NormalMaterialU   = YMaterial "NormalColor" "NormalTexture"

type GeoGlobalUniforms = PerspectiveUniforms ++ '[ YZFarPlane ]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix] ++ AlbedoMaterialU ++ NormalMaterialU
type GeoVertex         = P3TX2NT3

data GeoPassChannels = GeoPassChannels
    { gAlbedoChannel :: Texture
    , gNormalChannel :: Texture
    , gDepthChannel  :: Texture
    }

data GeoMaterial = GeoMaterial
    { albedoMaterial :: Material
    , normalMaterial :: Material
    }

type GeoEntity = SceneEntity GeoVertex GeoMaterial
type GeometryPass = PassDescr String GeoPassChannels GeoEntity GeoGlobalUniforms GeoLocalUniforms

geoPass :: ViewportI -> SScene GeoVertex GeoMaterial lit -> GeometryPass
geoPass viewport scene = PassDescr
    { passTarget         = geoTarget
    , passShader         = ShaderResource "res/glsl/pass/geoPass.vert" "res/glsl/pass/geoPass.frag"
    , passGlobalUniforms = geoUniforms
    , passEntityUniforms = entityUniforms
    , passGlobalTextures = []
    , passPreRendering   = io $ do
        GL.viewport     GL.$= toGLViewport viewport
        GL.clearColor   GL.$= GL.Color4 0 0 0 0
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

        -- GL.polygonMode  GL.$= (GL.Line, GL.Line)
        GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
    , passPostRendering  = return ()
    }

    where
    sceneCam        = scene^.sceneCamera
    glvp            = fromIntegral <$> viewport
    baseSpec        = mkTextureSpec' (glvp^.vpSize) GL.RGBA
    depthSpec       = mkTextureSpec' (glvp^.vpSize) GL.DepthComponent
    
    albedoT         = TextureBuffer "gbuffer-albedo" GL.Texture2D baseSpec
    normalT         = TextureBuffer "gbuffer-normal" GL.Texture2D baseSpec
    --specularTex     = TextureBuffer "gbuffer-specul" Texture2D $ GLBufferSpec RGB8 size
    --glossyTex       = TextureBuffer "gbuffer-glossy" Texture2D $ GLBufferSpec RGB8 size
    depthBuff       = TextureBuffer "gbuffer-depth" GL.Texture2D  depthSpec
    geoTarget       = RenderTarget "geo-fbo" $
                        GeoPassChannels 
                           { gAlbedoChannel = albedoT
                           , gNormalChannel = normalT
                           , gDepthChannel  = depthBuff
                           }
    
    geoUniforms =
        let zfar  = - (realToFrac $ sceneCam^.cameraPlanes.camZFar)
        in perspectiveUniforms glvp sceneCam  <+>
           zFarPlane        =: zfar
    
    entityUniforms ent =
        let modelM       = calcModelMatrix (ent^.transformation)
            -- TODO rethink the normal matrix here
            normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) {--<|> Just eye3--}) ^?!_Just
        in modelMatrix       =: ((fmap . fmap) realToFrac modelM)           <+>
           normalMatrix      =: ((fmap . fmap) realToFrac normalM)          <+>
           materialUniforms 0 (albedoMaterial $ ent^.material)              <+>
           materialUniforms 1 (normalMaterial $ ent^.material)

instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannels{gAlbedoChannel, gNormalChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget GL.Texture2D gNormalChannel 0
        ] 
    
    fboDepth GeoPassChannels{gDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D gDepthChannel 0
