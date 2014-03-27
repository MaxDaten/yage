{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Yage.Pipeline.Deferred.Common
import qualified Graphics.Rendering.OpenGL as GL


type GeoGlobalUniforms = PerspectiveUniforms ++ [YZFarPlane, YAlbedoTex, YNormalTex]
type GeoLocalUniforms  = [YModelMatrix, YNormalMatrix] ++ YMaterial
type GeoVertex         = P3TX2NT3

data GeoPassChannels = GeoPassChannels
    { gAlbedoChannel :: Texture
    , gNormalChannel :: Texture
    , gDepthChannel  :: Texture
    }


type GeometryPass = PassDescr String GeoPassChannels (SceneEntity GeoVertex) GeoGlobalUniforms GeoLocalUniforms

geoPass :: ViewportI -> SScene GeoVertex lit -> GeometryPass
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
    
    albedoT         = TextureBuffer "gbuffer-albedo" Texture2D $ GLBufferSpec RGBA8 (glvp^.vpSize)
    normalT         = TextureBuffer "gbuffer-normal" Texture2D $ GLBufferSpec RGBA8 (glvp^.vpSize)
    --specularTex     = TextureBuffer "gbuffer-specul" Texture2D $ GLBufferSpec RGB8 size
    --glossyTex       = TextureBuffer "gbuffer-glossy" Texture2D $ GLBufferSpec RGB8 size
    depthBuff       = TextureBuffer "gbuffer-depth" Texture2D  $ GLBufferSpec DepthComponent32 (glvp^.vpSize)
    geoTarget       = RenderTarget "geo-fbo" $
                        GeoPassChannels 
                           { gAlbedoChannel = albedoT
                           , gNormalChannel = normalT
                           , gDepthChannel  = depthBuff
                           }
    
    geoUniforms =
        let zfar  = - (realToFrac $ sceneCam^.cameraPlanes.camZFar)
        in perspectiveUniforms glvp sceneCam  <+>
           zFarPlane        =: zfar    <+>
           albedoTex        =: 0       <+>
           normalTex        =: 1
    
    entityUniforms ent =
        let modelM       = calcModelMatrix (ent^.transformation)
            -- TODO rethink the normal matrix here
            normalM      = (adjoint <$> (inv33 . fromTransformation $ modelM) {--<|> Just eye3--}) ^?!_Just
        in modelMatrix       =: ((fmap . fmap) realToFrac modelM)           <+>
           normalMatrix      =: ((fmap . fmap) realToFrac normalM)          <+>
           materialBaseColor =: (realToFrac <$> ent^.material.matBaseColor) <+>
           materialSpecular  =: (realToFrac $ ent^.material.matSpecular)


instance FramebufferSpec GeoPassChannels RenderTargets where
    fboColors GeoPassChannels{gAlbedoChannel, gNormalChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget Texture2D gAlbedoChannel 0
        , Attachment (ColorAttachment 1) $ TextureTarget Texture2D gNormalChannel 0
        ] 
    
    fboDepth GeoPassChannels{gDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget Texture2D gDepthChannel 0
