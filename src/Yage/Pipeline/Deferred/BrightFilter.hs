{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Yage.Pipeline.Deferred.BrightFilter where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry as Geometry
import Yage.Uniforms as U

import Yage.Viewport
import Yage.Scene
import Yage.TH.Shader as GLSL

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type BrightUniforms = '[ YWhitePoint ]
type BrightTextures = '[ TextureSampler "TextureSamplers[0]" ]

type BrightFrameData=ShaderData BrightUniforms BrightTextures
type BrightPass     = YageTextureSampler SingleRenderTarget BrightUniforms BrightTextures


-------------------------------------------------------------------------------
-- | Fragment code
fragmentProgram :: GLSL.ShaderSource FragmentShader
fragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

uniform float WhitePoint;

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec3 texColor = texture( TextureSamplers[0], SamplingUV[0] ).rgb;
    vec3 color = vec3(0.0);

    if (length(texColor) >= WhitePoint)
    {
        color = texColor;
    }

    pixelColor = color;
}
|]
-------------------------------------------------------------------------------


brightFilter :: Texture -> Float -> RenderSystem Texture
brightFilter tex whitePoint =
    let target      = mkSingleTextureTarget $ tex & textureId <>~ "-brightPass"

        brightDescr :: BrightPass
        brightDescr = samplerPass "brightFilter" target (target^.asRectangle) fragmentProgram


        frameData   :: BrightFrameData
        frameData   = ShaderData RNil RNil
                        & shaderTextures <<+>~ textureSampler =: tex
                        & shaderUniforms <<+>~ U.whitePoint =: realToFrac whitePoint

        brightPass  = runRenderPass brightDescr
    in do
        frameData `brightPass` ( singleton targetQuad )
        return $ target^.targetTexture


