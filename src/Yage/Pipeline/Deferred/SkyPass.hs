{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Yage.Pipeline.Deferred.SkyPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Rendering
import           Yage.Geometry

import           Yage.Scene
import           Yage.Viewport
import           Yage.Uniforms
import           Yage.Material
import           Yage.HDR
import           Yage.TH.Shader                     as GLSL

import           Yage.Pipeline.Deferred.Common

import qualified Graphics.Rendering.OpenGL          as GL


type SkyEntityUni      = '[ YModelMatrix ] ++ YSkyMaterial

type SkyVertex         = Vertex (Y'P3 GLfloat)

type SkyUni = PerspectiveUniforms ++ SkyEntityUni
type SkyTextures = '[ YSkyTex ]
type SkyShader = Shader SkyUni SkyTextures SkyVertex

data SkyInChannels = SkyInChannels
    { sBufferChannel :: Texture
    , sDepthChannel  :: Texture
    }

type SkyPass = PassDescr SkyInChannels SkyShader


type SkyMaterial = Material MaterialColorAlpha

type SkyEntity = Entity (Mesh SkyVertex) SkyMaterial


-------------------------------------------------------------------------------
-- | Vertex GLSL
skyVertexProgram :: GLSL.ShaderSource VertexShader
skyVertexProgram = [GLSL.yVertex|
#version 410 core

uniform mat4 ViewMatrix        = mat4(1.0);
uniform mat4 VPMatrix          = mat4(1.0);
uniform mat4 ModelMatrix       = mat4(1.0);
uniform mat4 SkyTextureMatrix  = mat4(1.0);

in vec3 vPosition;

out vec3 VertexSTP;

mat4 MVPMatrix = VPMatrix * ModelMatrix;
void main()
{
    mat4 MVPMatrix  = VPMatrix * ModelMatrix;
    VertexSTP       = (SkyTextureMatrix * vec4(vPosition, 1.0)).stp;
    gl_Position     = MVPMatrix * vec4( vPosition, 1.0 );
}
|]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- | Fragment GLSL
skyFragmentProgram :: GLSL.ShaderSource FragmentShader
skyFragmentProgram = [GLSL.yFragment|
#version 410 core


uniform samplerCube SkyTexture;
uniform vec4 SkyColor;
in vec3 VertexSTP;

layout (location = 0) out vec4 pixelColor;

void main()
{
    pixelColor       = SkyColor * texture(SkyTexture, VertexSTP);
}
|]

-------------------------------------------------------------------------------



skyPass :: RenderTarget SkyInChannels -> Viewport Int -> SkyPass
skyPass target viewport =
    passPreset target (viewport^.rectangle) (ShaderUnit shaderProg)
        & passPreRendering .~ preRendering

    where

    shaderProg = ShaderProgramUnit
                    { _shaderName    = "SkyPass.hs"
                    , _shaderSources = [ skyVertexProgram^.shaderSource
                                       , skyFragmentProgram^.shaderSource
                                       ]
                    }

    preRendering   = io $ do
        GL.viewport     GL.$= viewport^.glViewport

        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled

        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        -- we are looking from the inside into the sky box direction
        GL.frontFace    GL.$= GL.CW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)


skyFrameData :: Viewport Int -> Camera -> ShaderData PerspectiveUniforms '[]
skyFrameData viewport camera = ShaderData (perspectiveUniforms viewport camera) mempty


toSkyEntity :: SkyEntity -> RenderEntity SkyVertex (ShaderData SkyEntityUni SkyTextures)
toSkyEntity sky = toRenderEntity shData sky
    where
    shData   = ShaderData uniforms RNil `append` material
    uniforms = modelMatrix =: ( sky^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )

    material :: YSkyData
    material = materialUniformsColor $ sky^.materials


instance FramebufferSpec SkyInChannels RenderTargets where
    fboColors SkyInChannels{sBufferChannel} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D sBufferChannel 0
        ]

    fboDepth SkyInChannels{sDepthChannel} =
        Just $ Attachment DepthAttachment $ TextureTarget GL.Texture2D sDepthChannel 0

