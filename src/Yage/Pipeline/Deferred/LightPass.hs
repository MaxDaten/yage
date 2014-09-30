{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Yage.Material
import Yage.TH.Shader

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.GeometryPass

import qualified Graphics.Rendering.OpenGL as GL

type LitPerFrameUni     = PerspectiveUniforms ++ [ YViewportDim, YZProjRatio, YGamma, YViewToWorldMatrix ]
type LitPerFrameTex     = [ YAlbedoTex, YNormalTex, YDepthTex, YEnvironmentCubeMap ]

type LitPerEntityUni    = '[ YModelMatrix ] ++ YLightAttributes

type LitVertex          = Vertex (Y'P3 GLfloat)

data LitPassChannels = LitPassChannels
    { lBufferChannel :: Texture
    , lDepthChannel  :: Texture
    }

type LitUni = LitPerFrameUni ++ LitPerEntityUni
type LitTex = LitPerFrameTex
type LitShader = Shader LitUni LitTex LitVertex

type LightPass = YageDeferredPass LitPassChannels LitShader

type LitEntityT f    = LightEntity (f LitVertex)
type LitEntityRes    = LitEntityT Mesh
type LitEntityDraw   = LitEntityT Mesh

type LitPassScene ent sky = Scene ent (Environment LitEntityDraw sky)

lightPass :: GeometryPass -> Viewport Int -> (Environment LitEntityDraw sky) -> LightPass
lightPass base viewport environment =
    passPreset target (viewport^.rectangle) (ShaderUnit shaderProg)
       & passPreRendering .~ preRendering

    where

    target =
        let GeoPassChannelsF{..} = renderTargets base
        in RenderTarget "light-fbo" $ LitPassChannels
                                { lBufferChannel   = lightTex
                                , lDepthChannel    = gDepthChannel
                                }

    lightSpec       = mkTextureSpec (viewport^.rectangle.extend) GL.Float GL.RGBA GL.RGBA16F
    lightTex        = mkTargetTexture "lbuffer" lightSpec

    shaderProg = ShaderProgramUnit
                    { _shaderName    = "LightPass.hs"
                    , _shaderSources = [ $(vertexFile "res/glsl/pass/lightPass.vert")^.shaderSource
                                       , $(fragmentFile "res/glsl/pass/lightPass.frag")^.shaderSource
                                       ]
                    }
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



litPerFrameData :: GeometryPass -> Viewport Int -> Camera -> RenderMaterial MaterialColorAlpha  -> ShaderData LitPerFrameUni LitPerFrameTex
litPerFrameData base viewport camera envMat = ShaderData lightUniforms attributeTextures
    where

    lightUniforms   :: Uniforms LitPerFrameUni
    lightUniforms   =
        let (V2 near far)           = realToFrac <$> V2 (-camera^.cameraPlanes.camZNear) (-camera^.cameraPlanes.camZFar)
            zProj                   = V2 ( far / ( far - near ) ) ( ( (-far) * near ) / ( far - near ) )
            dim                     = fromIntegral <$> viewport^.rectangle.extend
            theGamma                = realToFrac $ viewport^.viewportGamma
            invCam                  = camera & cameraTransformation %~ inverseTransformation
        in
        perspectiveUniforms viewport camera     <+>
        viewportDim         =: dim              <+>
        zProjRatio          =: zProj            <+>
        gamma               =: theGamma         <+>
        viewToWorldMatrix   =: (fmap realToFrac <$> invCam^.cameraMatrix^.to m44_to_m33)

    attributeTextures :: Textures LitPerFrameTex
    attributeTextures =
        textureSampler =: (gAlbedoChannel $ renderTargets base)    <+>
        textureSampler =: (gNormalChannel $ renderTargets base)    <+>
        textureSampler =: (gDepthChannel  $ renderTargets base)    <+>
        textureSampler =: (extract $ envMat^.matTexture)




mkLight :: Light -> LitEntityRes
mkLight light =
    let vol           = lightData
        lightEnt     = Entity vol () idTransformation (GLDrawSettings GL.Triangles (Just GL.Front))
    in LightEntity lightEnt light
    where
    lightData :: Mesh LitVertex
    lightData = case lightType light of
        Pointlight{}      -> mkFromVerticesF "plight" . map (position3 =:) . vertices . triangles $ geoSphere 2 1
        Spotlight{}       -> error "Yage.Pipeline.Deferred.Light.mkLight: Spotlight not supported"
        OmniDirectional   -> error "Yage.Pipeline.Deferred.Light.mkLight: OmniDirectional not supported"


instance FramebufferSpec LitPassChannels RenderTargets where
    fboColors LitPassChannels{lBufferChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D lBufferChannel 0
        ]

    fboDepth LitPassChannels{lDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D lDepthChannel 0



toLitEntity :: LitEntityDraw -> RenderEntity LitVertex (ShaderData LitPerEntityUni '[])
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
        Pointlight{..}    -> U.lightPosition =: (realToFrac <$> ent^.entityPosition )                   <+>
                             U.lightRadius   =: (realToFrac <$> ent^.entityScale )                      <+>
                             U.lightColor    =: (realToFrac <$> lAttrColor lightAttr )                  <+>
                             U.lightAtten    =: (realToFrac <$> V3 attConst attLinear attQuad )         <+>
                             U.lightSpecExp  =: (realToFrac  $ lAttrSpecularExp lightAttr )
        Spotlight{..}     -> error "Yage.Pipeline.Deferred.Light.lightAttributes: Spotlight not supported"
        OmniDirectional   -> error "Yage.Pipeline.Deferred.Light.lightAttributes: OmniDirectional not supported"

