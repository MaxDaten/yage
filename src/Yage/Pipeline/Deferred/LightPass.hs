{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Pipeline.Deferred.LightPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry as Geometry
import Yage.Geometry3D
import Yage.Uniforms as U
import Yage.Camera
import Yage.Scene

import Yage.Rendering hiding (P3)
import Yage.Rendering.Textures              (mkTextureSpec')

import Yage.Rendering.Transformation

import Yage.Pipeline.Deferred.GeometryPass

import qualified Graphics.Rendering.OpenGL as GL


type LitGlobalUniforms = PerspectiveUniforms ++ [YViewportDim, YZNearFarPlane, YZProjRatio, YAlbedoTex, YNormalTex, YDepthTex]
type LitLocalUniforms  = '[YModelMatrix] ++ YLightAttributes
type LitVertex         = P3

data LightPassChannels = LightPassChannels
    { lBufferChannel :: Texture 
    , lDepthChannel  :: Texture
    }

type LightPass = PassDescr String LightPassChannels (SceneLight LitVertex) LitGlobalUniforms LitLocalUniforms

lightPass :: GeometryPass -> ViewportI -> SScene geo mat LitVertex -> LightPass
lightPass base viewport scene =
    let GeoPassChannels{..} = renderTargets base
    in PassDescr
    { passTarget         = RenderTarget "light-fbo" $
                            LightPassChannels
                            { lBufferChannel   = lightTex
                            , lDepthChannel    = gDepthChannel
                            }
    , passShader         = ShaderResource "res/glsl/pass/lightPass.vert" "res/glsl/pass/lightPass.frag"
    , passGlobalUniforms = lightPassUniforms
    , passEntityUniforms = lightUniforms
    , passGlobalTextures = [ TextureDefinition (0, "albedo") gAlbedoChannel
                           , TextureDefinition (1, "normal") gNormalChannel
                           , TextureDefinition (2, "depth") gDepthChannel
                           ]
    , passPreRendering   = io $ do
        GL.viewport     GL.$= toGLViewport viewport
        let AmbientLight ambientColor = scene^.sceneEnvironment.envAmbient
            V3 r g b                  = realToFrac <$> ambientColor
        GL.clearColor   GL.$= GL.Color4 r g b 0
        
        GL.depthFunc    GL.$= Nothing           -- disable func add
        GL.depthMask    GL.$= GL.Disabled       -- writing to depth is disabled
        
        GL.blend        GL.$= GL.Enabled        --- could reject background frags!
        GL.blendEquation GL.$= GL.FuncAdd
        GL.blendFunc    GL.$= (GL.One, GL.One)

        -- we reject front faces because of culling if cam is in volume 
        GL.cullFace     GL.$= Just GL.Front
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)
        
        GL.clear        [ GL.ColorBuffer ]
        -- print "setup correct lighting params"
    , passPostRendering  = return ()
    }

    where
    
    sceneCam        = scene^.sceneCamera
    vpgl            = fromIntegral <$> viewport
    outSpec         = mkTextureSpec' (viewport^.vpSize) GL.RGB
    
    lightTex        = TextureBuffer "lbuffer" GL.Texture2D outSpec

    lightPassUniforms =
        let zNearFar@(V2 near far)      = realToFrac <$> V2 (-sceneCam^.cameraPlanes^.camZNear) (-sceneCam^.cameraPlanes^.camZFar)
            zProj                       = V2 ( far / (far - near)) ((-far * near) / (far - near))
            fvp                         = vpgl
            dim                         = vpgl^.vpSize
        in
        perspectiveUniforms fvp sceneCam     <+>
        viewportDim      =: dim              <+>
        zNearFarPlane    =: zNearFar         <+>
        zProjRatio       =: zProj            <+>
        albedoTex        =: 0                <+>
        normalTex        =: 1                <+>
        depthTex         =: 2


    lightUniforms :: SceneLight a -> Uniforms LitLocalUniforms
    lightUniforms light = 
        let modelM       = calcModelMatrix $ light^.lightTransformation
            lightProps   = light^.lightProperties
        in 
        modelMatrix =: ((fmap.fmap) realToFrac modelM)  <+>
        lightAttributes ( lightProps )


mkLight :: Light -> SceneLight LitVertex
mkLight light = 
    let (vol, trans) = lightData
    in SceneLight vol trans light (GLDrawSettings Triangles (Just Front))
    where
    lightData = case lightType light of
        p@Pointlight{}    -> pLightVolume p
        Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.mkLight: Spotlight not supported"
        OmniDirectional _ -> error "Yage.Pipeline.Deferred.Light.mkLight: OmniDirectional not supported"
    
    pLightVolume Pointlight{..} 
        = ( meshFromVertexList "plight" . vertices . triangles $ geoSphere 2 1
          , idTransformation & transPosition .~ (realToFrac <$> pLightPosition)
                             & transScale    .~ (realToFrac <$> pLightRadius)
          )
    pLightVolume _ = error "Yage.Pipeline.Deferred.Light.mkLight: unsupported light type" 




lightAttributes :: Light -> Uniforms YLightAttributes
lightAttributes Light{lightType,lightAttribs} = case lightType of
    Pointlight{..}    -> U.lightPosition =: (realToFrac <$> pLightPosition)                    <+>
                         U.lightRadius   =: (realToFrac <$> pLightRadius)                      <+>
                         U.lightColor    =: (realToFrac <$> lAttrColor lightAttribs)           <+>
                         U.lightAtten    =: (realToFrac <$> lAttrAttenuation lightAttribs)     <+>
                         U.lightSpecExp  =: (realToFrac $ lAttrSpecularExp lightAttribs)
    Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.lightAttributes: Spotlight not supported"
    OmniDirectional _ -> error "Yage.Pipeline.Deferred.Light.lightAttributes: OmniDirectional not supported"


instance FramebufferSpec LightPassChannels RenderTargets where
    fboColors LightPassChannels{lBufferChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D lBufferChannel 0
        ] 
    
    fboDepth LightPassChannels{lDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D lDepthChannel 0