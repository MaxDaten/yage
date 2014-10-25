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

type ScreenTex    = [ TextureSampler "TextureSamplers[0]"
                    , TextureSampler "TextureSamplers[1]"
                    ]

type ScreenShader = Shader '[] ScreenTex TargetVertex

type ScreenPass   = YageDeferredPass DefaultRenderTarget ScreenShader


-------------------------------------------------------------------------------
-- | Fragment code
screenFragmentProgram :: GLSL.ShaderSource FragmentShader
screenFragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

layout (location = 0) out vec3 pixelColor;

void main(void)
{
    vec3 baseColor = texture( TextureSamplers[0], SamplingUV[0] ).rgb;
    vec4 addColor  = texture( TextureSamplers[1], SamplingUV[1] );
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
    in screenData `runPass` ( singleton targetQuad )

    where

    screenFrameData :: [ Texture ] -> ShaderData '[] ScreenTex
    screenFrameData (tex0:tex1:[]) =
        let sampler = textureSampler =: tex0 <+> textureSampler =: tex1
        in ShaderData mempty sampler
    screenFrameData _ = error "invalid texture argument count"


