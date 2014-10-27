{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Yage.Pipeline.Deferred.DownsamplePass where

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

type DownsampleUniforms = '[ YTextureSize "TextureSize[0]" ]
type DownsampleTextures = '[ TextureSampler "TextureSamplers[0]" ]

type DownsampleFrameData= ShaderData DownsampleUniforms DownsampleTextures
type DownsamplePass     = YageTextureSampler SingleRenderTarget DownsampleUniforms DownsampleTextures

-------------------------------------------------------------------------------
-- | Fragment code
downFragmentProgram :: GLSL.ShaderSource FragmentShader
downFragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec2 offset = TextureSize[0].zw;
    vec2 uv[4];

    uv[0] = SamplingUV[0] + offset * vec2(-1, -1);
    uv[1] = SamplingUV[0] + offset * vec2( 1, -1);
    uv[2] = SamplingUV[0] + offset * vec2(-1,  1);
    uv[3] = SamplingUV[0] + offset * vec2( 1,  1);

    vec3 sampleColor = texture( TextureSamplers[0], SamplingUV[0] ).rgb;
    for (uint i = 0; i < 4; ++i)
    {
        sampleColor += texture( TextureSamplers[0], uv[i] ).rgb;
    }

    pixelColor = sampleColor * 0.2;
}
|]
-------------------------------------------------------------------------------

downsampleBoxed5x5 :: Int -> Texture -> RenderSystem Texture
downsampleBoxed5x5 downfactor toDownsample =
    let outSize  = liftA (`div` downfactor) $ toDownsample^.textureSpec.texSpecDimension
        target   = mkSingleTargetFromSpec ( toDownsample^.textureId ++ downfactor^.to show.packedChars )
                                          ( toDownsample^.textureSpec & texSpecDimension .~ outSize )

        downsampleDescr :: DownsamplePass
        downsampleDescr = samplerPass "Yage.DownsamplePass" target (target^.asRectangle) downFragmentProgram


        downsampleData :: DownsampleFrameData
        downsampleData = sampleData toDownsample

        downsamplePass = runRenderPass downsampleDescr
    in do
        downsampleData `downsamplePass` ( targetQuad^.to singleton )
        return $ target^.targetTexture

