{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.ScreenPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Geometry

import           Yage.Scene
import           Yage.Uniforms                      as U
import           Yage.Viewport                      as VP
import           Yage.TH.Shader                     as GLSL

import           Yage.Rendering

import           Yage.Pipeline.Deferred.Common
import           Yage.Pipeline.Deferred.Sampler
import qualified Graphics.Rendering.OpenGL          as GL


newtype Screen = Screen (Viewport Int)

type ScrPerFrameUni    = [ YProjectionMatrix
                         , YTextureSize "TextureSize0"
                         , YTextureSize "TextureSize1"
                         ]

type ScrVertex    = Vertex (Y'P3TX2 GLfloat)
type ScreenUni    = ScrPerFrameUni ++ '[ YModelMatrix ]
type ScreenTex    = [ TextureUniform "TextureSampler0"
                    , TextureUniform "TextureSampler1"
                    ]

type ScreenShader = Shader ScreenUni ScreenTex ScrVertex

type ScreenPass   = YageDeferredPass DefaultRenderTarget ScreenShader


-------------------------------------------------------------------------------
-- | Fragment code
screenFragmentProgram :: GLSL.ShaderSource FragmentShader
screenFragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

in vec2 SamplingUV;
layout (location = 0) out vec3 pixelColor;


void main(void)
{
    vec3 baseColor = texture( TextureSampler0, SamplingUV0 ).rgb;
    vec4 addColor  = texture( TextureSampler1, SamplingUV1 );
    vec3 outColor  = mix( baseColor, addColor.rgb, addColor.a );

    pixelColor = clamp(outColor, 0, 1);
}
|]
-------------------------------------------------------------------------------


screenPass :: YageRenderSystem [ Texture ] ()
screenPass viewport textures =
    let screenPassDescr :: ScreenPass
        screenPassDescr =
            (samplerPass "Yage.ScreenPass" defaultRenderTarget (viewport^.rectangle) screenFragmentProgram)
                 & passPreRendering .~ ( io $ do
                    -- our 0/0 is top left (y-Axis is flipped)
                    GL.viewport     GL.$= viewport^.glViewport
                    GL.clearColor   GL.$= GL.Color4 1 1 1 0

                    GL.depthFunc    GL.$= Nothing    -- TODO to init
                    GL.depthMask    GL.$= GL.Disabled

                    GL.blend        GL.$= GL.Disabled

                    GL.cullFace     GL.$= Just GL.Back
                    GL.frontFace    GL.$= GL.CCW
                    GL.polygonMode  GL.$= (GL.Fill, GL.Fill)
                    )


        runPass     = runRenderPass screenPassDescr
        screenData  = screenFrameData textures
    in screenData `runPass` [ targetEntity $ viewport^.rectangle ]

    where

    screenFrameData :: [ Texture ] -> ShaderData ScrPerFrameUni ScreenTex
    screenFrameData (tex0:tex1:[]) =
        targetRectangleData (viewport^.rectangle) `append`
        sampleData tex0 `append`
        sampleData tex1
    screenFrameData _ = error "invalid texture argument count"


instance (Implicit (FieldNames ScreenTex)) where
    implicitly =
        SField =: "TextureSampler0" <+>
        SField =: "TextureSampler1"

