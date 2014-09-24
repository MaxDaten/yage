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

type DownsampleUniforms = [ YProjectionMatrix
                          , YTextureSize "TextureSize0"
                          , YModelMatrix
                          ]
type DownsampleTextures = '[ TextureUniform "TextureSampler0" ]
type DownsamplePass     = YageTextureSampler SingleRenderTarget DownsampleUniforms DownsampleTextures

-------------------------------------------------------------------------------
-- | Fragment code
fragmentProgram :: GLSL.ShaderSource FragmentShader
fragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec2 offset = TextureSize0.zw;
    vec2 uv[4];

    uv[0] = SamplingUV0 + offset * vec2(-1, -1);
    uv[1] = SamplingUV0 + offset * vec2( 1, -1);
    uv[2] = SamplingUV0 + offset * vec2(-1,  1);
    uv[3] = SamplingUV0 + offset * vec2( 1,  1);

    vec3 sampleColor = vec3(0);
    for (uint i = 0; i < 4; ++i)
    {
        sampleColor += texture( TextureSampler0, uv[i] ).rgb;
    }

    pixelColor = sampleColor * 0.25;
}
|]
-------------------------------------------------------------------------------

downsampleBoxed5x5 :: Int -> Texture -> RenderSystem Texture
downsampleBoxed5x5 downfactor toDownsample =
    let outSize  = liftA (`div` downfactor) $ toDownsample^.textureSpec.texSpecDimension
        target   = mkSingleTargetFromSpec ( toDownsample^.textureId ++ downfactor^.to show.packedChars )
                                          ( toDownsample^.textureSpec & texSpecDimension .~ outSize )

        downsampleDescr :: DownsamplePass
        downsampleDescr = samplerPass "Yage.DownsamplePass" target (target^.asRectangle) fragmentProgram


        downsampleData :: ShaderData [ YProjectionMatrix, YTextureSize "TextureSize0"] '[ TextureUniform "TextureSampler0" ]
        downsampleData = targetRectangleData (target^.asRectangle) `append`
                         sampleData toDownsample

        downsamplePass = runRenderPass downsampleDescr
    in do
        downsampleData `downsamplePass` [ targetEntity target ]
        return $ target^.targetTexture


instance Implicit (FieldNames '[ TextureUniform "TextureSampler0" ]) where
    implicitly = SField =: "TextureSampler0"
