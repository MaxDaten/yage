{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Yage.Pipeline.Deferred.GlareDetectionPass where

import Yage.Prelude
import Yage.Lens

import Control.Applicative (liftA)
import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.TH.Shader as GLSL

import Yage.Rendering
import Yage.Rendering.Textures (texSpecDimension)

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type GlarePerFrameUni = [ YProjectionMatrix
                        , YTextureSize "TextureSize[0]"
                        , YExposure
                        , YBloomThreshold
                        ]
type GlareUniforms = GlarePerFrameUni ++ '[ YModelMatrix ]
type GlareTextures = '[ TextureSampler "TextureSamplers[0]" ]

type GlareFrameData= ShaderData GlarePerFrameUni GlareTextures
type GlarePass     = YageTextureSampler SingleRenderTarget GlareUniforms GlareTextures

-------------------------------------------------------------------------------
-- | Fragment code
glareFragmentProgram :: GLSL.ShaderSource FragmentShader
glareFragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

uniform float Exposure;
uniform float BloomThreshold;

layout (location = 0) out vec4 pixelColor;

void main()
{
    vec2 offset = TextureSize[0].zw;
    vec2 uv[5];

    uv[0] = SamplingUV[0] + offset * vec2( 0,  0);
    uv[1] = SamplingUV[0] + offset * vec2(-1,  0);
    uv[2] = SamplingUV[0] + offset * vec2( 0, -1);
    uv[3] = SamplingUV[0] + offset * vec2( 1,  0);
    uv[4] = SamplingUV[0] + offset * vec2( 0,  1);

    vec4 sampleColor = vec4(1e100);
    // "search the minimum color value from the box-filter area"
    for (uint i = 0; i < 5; ++i)
    {
        sampleColor = min(texture( TextureSamplers[0], uv[i] ), sampleColor);
    }
    sampleColor.rgb *= Exposure;

    pixelColor = vec4(max(sampleColor.rgb - BloomThreshold / (1.0 - BloomThreshold), 0.0), sampleColor.a);
}
|]
-------------------------------------------------------------------------------

-- | a glare detection algorithm with an integrated 5x5 box downsample
glareDetection :: Int -> Float -> Float -> Texture -> RenderSystem Texture
glareDetection downfactor exposure bloomThreshold toDownsample =
    let outSize  = liftA (`div` downfactor) $ toDownsample^.textureSpec.texSpecDimension
        target   = mkSingleTargetFromSpec ( toDownsample^.textureId ++ downfactor^.to show.packedChars )
                                          ( toDownsample^.textureSpec & texSpecDimension .~ outSize )

        glareDescr :: GlarePass
        glareDescr = samplerPass "Yage.GlareDetection" target (target^.asRectangle) glareFragmentProgram


        glareData :: GlareFrameData
        glareData = (targetRectangleData (target^.asRectangle) `append`
                        sampleData toDownsample)
                        & shaderUniforms <<+>~ U.exposure       =: realToFrac exposure
                        & shaderUniforms <<+>~ U.bloomthreshold =: realToFrac bloomThreshold


        glarePass = runRenderPass glareDescr
    in do
        glareData `glarePass` [ targetEntity target ]
        return $ target^.targetTexture

