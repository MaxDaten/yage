{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
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

import Yage.Pipeline.Deferred.GeometryPass

import qualified Graphics.Rendering.OpenGL as GL

type LitPerFrameUni     = PerspectiveUniforms ++ [ YViewportDim, YZNearFarPlane, YZProjRatio ]
type LitPerFrameTex     = [ YAlbedoTex, YNormalTex, YDepthTex ]
type LitPerFrame        = ShaderData LitPerFrameUni LitPerFrameTex

type LitPerEntityUni    = '[ YModelMatrix ] ++ YLightAttributes
type LitPerEntityTex    = '[ ]
type LitPerEnity        = ShaderData LitPerEntityUni LitPerEntityTex

type LitVertex          = Y'P3 GLfloat

data LitPassChannels = LitPassChannels
    { lBufferChannel :: Texture 
    , lDepthChannel  :: Texture
    }

type LightPass = PassDescr
                    LitPassChannels
                    LitPerFrame
                    LitPerEnity
                    LitVertex

type LitEntityT f    = LightEntity (f LitVertex)
type LitEntityRes    = LitEntityT Mesh
type LitEntityDraw   = LitEntityT Mesh

type LitPassScene ent sky = Scene ent (Environment LitEntityDraw sky)

lightPass :: GeometryPass -> ViewportI -> LitPassScene ent sky -> LightPass
lightPass base viewport scene =
    let GeoPassChannels{..} = renderTargets base
    in PassDescr
    { passTarget         = RenderTarget "light-fbo" $ LitPassChannels
                                { lBufferChannel   = lightTex
                                , lDepthChannel    = gDepthChannel
                                }
    , passShader         = ShaderResource "res/glsl/pass/lightPass.vert" "res/glsl/pass/lightPass.frag"
    , passPerFrameData   = lightShaderData
    -- , passGlobalTextures = [ TextureDefinition (0, "albedo") gAlbedoChannel
    --                       , TextureDefinition (1, "normal") gNormalChannel
    --                       , TextureDefinition (2, "depth") gDepthChannel
    --                       ]
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
    
    lightTex        = Texture "lbuffer" $ TextureBuffer GL.Texture2D outSpec

    lightShaderData :: LitPerFrame
    lightShaderData = ShaderData lightUniforms attributeTextures

    lightUniforms   :: Uniforms LitPerFrameUni
    lightUniforms   =
        let zNearFar@(V2 near far)      = realToFrac <$> V2 (-sceneCam^.cameraPlanes^.camZNear) (-sceneCam^.cameraPlanes^.camZFar)
            zProj                       = V2 ( far / (far - near)) ((-far * near) / (far - near))
            fvp                         = vpgl
            dim                         = vpgl^.vpSize
        in
        perspectiveUniforms fvp sceneCam     <+>
        viewportDim      =: dim              <+>
        zNearFarPlane    =: zNearFar         <+>
        zProjRatio       =: zProj

    attributeTextures :: Textures LitPerFrameTex
    attributeTextures =
        materialTexture =: (gAlbedoChannel $ renderTargets base)    <+>
        materialTexture =: (gNormalChannel $ renderTargets base)    <+>
        materialTexture =: (gDepthChannel  $ renderTargets base)


mkLight :: Light -> LitEntityRes
mkLight light = 
    let (vol, trans) = lightData
        lightEnt     = Entity vol () trans (GLDrawSettings GL.Triangles (Just GL.Front))
    in LightEntity lightEnt light 
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



instance FramebufferSpec LitPassChannels RenderTargets where
    fboColors LitPassChannels{lBufferChannel} = 
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D lBufferChannel 0
        ] 
    
    fboDepth LitPassChannels{lDepthChannel} = 
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D lDepthChannel 0



toLitEntity :: LitEntityDraw -> RenderEntity LitVertex LitPerEnity
toLitEntity (LightEntity ent light) = toRenderEntity ( ShaderData uniforms mempty ) ent
    where
    uniforms =
        modelMatrix =: theModelMatrix  <+>
        lightAttributes light
    
    theModelMatrix = (fmap.fmap) realToFrac $ calcModelMatrix $ ent^.transformation

    lightAttributes :: Light -> Uniforms YLightAttributes
    lightAttributes Light{lightType,lightAttribs} = case lightType of
        Pointlight{..}    -> U.lightPosition =: (realToFrac <$> pLightPosition)                    <+>
                             U.lightRadius   =: (realToFrac <$> pLightRadius)                      <+>
                             U.lightColor    =: (realToFrac <$> lAttrColor lightAttribs)           <+>
                             U.lightAtten    =: (realToFrac <$> lAttrAttenuation lightAttribs)     <+>
                             U.lightSpecExp  =: (realToFrac $ lAttrSpecularExp lightAttribs)
        Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.lightAttributes: Spotlight not supported"
        OmniDirectional _ -> error "Yage.Pipeline.Deferred.Light.lightAttributes: OmniDirectional not supported"

