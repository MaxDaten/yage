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
import Yage.Viewport
import Yage.Scene
import Yage.Transformation

import Yage.Rendering hiding (P3)
import Yage.Rendering.Textures              (mkTextureSpec)

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.GeometryPass

import qualified Graphics.Rendering.OpenGL as GL

type LitPerFrameUni     = PerspectiveUniforms ++ [ YViewportDim, YZNearFarPlane, YZProjRatio, YGamma ]
type LitPerFrameTex     = [ YAlbedoTex, YNormalTex, YDepthTex ]
type LitPerFrame        = ShaderData LitPerFrameUni LitPerFrameTex

type LitPerEntityUni    = '[ YModelMatrix ] ++ YLightAttributes
type LitPerEntityTex    = '[ ]
type LitPerEnity        = ShaderData LitPerEntityUni LitPerEntityTex

type LitVertex          = Vertex (Y'P3 GLfloat)

data LitPassChannels = LitPassChannels
    { lBufferChannel :: Texture 
    , lDepthChannel  :: Texture
    }

type LightPass = YageDeferredPass
                    LitPassChannels
                    LitPerFrame
                    LitPerEnity
                    LitVertex

type LitEntityT f    = LightEntity (f LitVertex)
type LitEntityRes    = LitEntityT Mesh
type LitEntityDraw   = LitEntityT Mesh

type LitPassScene ent sky = Scene ent (Environment LitEntityDraw sky)

lightPass :: GeometryPass -> Viewport Int -> Camera -> (Environment LitEntityDraw sky) -> LightPass
lightPass base viewport camera environment =
    let shaderRes   = ShaderResource "res/glsl/pass/lightPass.vert" "res/glsl/pass/lightPass.frag"
    in passPreset target (viewport^.rectangle) (shaderRes, shaderData)
       & passPreRendering .~ preRendering
    
    where
    
    target = 
        let GeoPassChannels{..} = renderTargets base
        in RenderTarget "light-fbo" $ LitPassChannels
                                { lBufferChannel   = lightTex
                                , lDepthChannel    = gDepthChannel
                                }
    
    lightSpec       = mkTextureSpec (viewport^.rectangle.extend) GL.Float GL.RGB GL.RGB32F
    lightTex        = mkTexture "lbuffer" $ TextureBuffer GL.Texture2D lightSpec

    shaderData :: LitPerFrame
    shaderData = ShaderData lightUniforms attributeTextures

    
    lightUniforms   :: Uniforms LitPerFrameUni
    lightUniforms   =
        let zNearFar@(V2 near far)  = realToFrac <$> V2 (-camera^.cameraPlanes.camZNear) (-camera^.cameraPlanes.camZFar)
            zProj                   = V2 ( far / (far - near)) ((-far * near) / (far - near))
            dim                     = fromIntegral <$> viewport^.rectangle.extend
            theGamma                = realToFrac $ viewport^.viewportGamma
        in
        perspectiveUniforms viewport camera     <+>
        viewportDim      =: dim              <+>
        zNearFarPlane    =: zNearFar         <+>
        zProjRatio       =: zProj            <+>
        gamma            =: theGamma

    attributeTextures :: Textures LitPerFrameTex
    attributeTextures =
        materialTexture =: (gAlbedoChannel $ renderTargets base)    <+>
        materialTexture =: (gNormalChannel $ renderTargets base)    <+>
        materialTexture =: (gDepthChannel  $ renderTargets base)
    
    preRendering   = io $ do
        GL.viewport     GL.$= viewport^.glViewport
        let AmbientLight ambientColor = environment^.envAmbient
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



mkLight :: Light -> LitEntityRes
mkLight light = 
    let vol           = lightData
        lightEnt     = Entity vol () idTransformation (GLDrawSettings GL.Triangles (Just GL.Front))
    in LightEntity lightEnt light 
    where
    lightData :: Mesh LitVertex
    lightData = case lightType light of
        Pointlight{}      -> mkFromVerticesF "plight" . vertices . triangles $ geoSphere 2 1
        Spotlight{}       -> error "Yage.Pipeline.Deferred.Light.mkLight: Spotlight not supported"
        OmniDirectional   -> error "Yage.Pipeline.Deferred.Light.mkLight: OmniDirectional not supported"


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
        modelMatrix =: ((fmap.fmap) realToFrac $ ent^.entityTransformation.transformationMatrix) <+>
        lightAttributes
    

    lightAttributes :: Uniforms YLightAttributes
    lightAttributes = 
        let lightAttr = lightAttribs light
            (attConst, attLinear, attQuad) = lAttrAttenuation lightAttr
        in case lightType light of
        Pointlight{..}    -> U.lightPosition =: (realToFrac <$> ent^.entityPosition )                 <+>
                             U.lightRadius   =: (realToFrac <$> ent^.entityScale )                    <+>
                             U.lightColor    =: (realToFrac <$> lAttrColor lightAttr )                <+>
                             U.lightAtten    =: (realToFrac <$> V3 attConst attLinear attQuad )       <+>
                             U.lightSpecExp  =: (realToFrac  $ lAttrSpecularExp lightAttr )
        Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.lightAttributes: Spotlight not supported"
        OmniDirectional   -> error "Yage.Pipeline.Deferred.Light.lightAttributes: OmniDirectional not supported"

