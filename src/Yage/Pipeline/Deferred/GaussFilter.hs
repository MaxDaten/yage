{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE QuasiQuotes    #-}
module Yage.Pipeline.Deferred.GaussFilter where

import Yage.Prelude
import Yage.Lens


import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP
import Yage.TH.Shader as GLSL

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type GaussUniforms  = [ YTextureSize "TextureSize[0]", YStepDirection, YGaussWidth ]
type GaussTextures  = '[ TextureSampler "TextureSamplers[0]" ]

type GaussFrameData = ShaderData GaussUniforms GaussTextures
type GaussShader    = Shader GaussUniforms GaussTextures TargetVertex
type GaussPass      = YageDeferredPass SingleRenderTarget GaussShader

type YGaussWidth    = "Width" ::: GLfloat

-------------------------------------------------------------------------------
-- | Fragment code
gaussBlurFragmentProgram :: GLSL.ShaderSource FragmentShader
gaussBlurFragmentProgram = [GLSL.yFragment|
#version 410 core
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

#include "pass/Sampling.frag"

uniform vec2 Direction;
uniform float Width = 1.0;

#define GAUSS_SAMPLES 13

#if     GAUSS_SAMPLES == 13
float offsets[13] = float[]( -1.7688, -1.1984, -0.8694, -0.6151, -0.3957, -0.1940, 0, 0.1940, 0.3957, 0.6151, 0.8694, 1.1984, 1.7688 );
const float n = 13.0;
#elif   GAUSS_SAMPLES == 7
float offsets[7] = float[]( -1.4652, -0.7916, -0.3661, 0, 0.3661, 0.7916, 1.4652 );
const float n = 7.0;
#endif

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec4 texColor = vec4(0);
    vec2 step = Width * Direction * TextureSize[0].zw;
    for (int i = 0; i < int(n); i++) {
        texColor += texture( TextureSamplers[0], SamplingUV[0] + offsets[i] * step);
    }
    pixelColor = texColor.rgb / n;
}
|]
-------------------------------------------------------------------------------

gaussFilter :: Double -> Texture -> (RenderTarget SingleRenderTarget, RenderTarget SingleRenderTarget) -> RenderSystem Texture
gaussFilter gwidth toSample (xTarget, yTarget) =
    let xPass, yPass :: GaussPass
        xPass       = samplerPass "Yage.GaussX" xTarget (xTarget^.asRectangle) gaussBlurFragmentProgram
        yPass       = samplerPass "Yage.GaussY" yTarget (yTarget^.asRectangle) gaussBlurFragmentProgram

        xData, yData :: GaussFrameData
        xData       = ShaderData RNil RNil
                        & shaderUniforms <<+>~ textureSizeField (xTarget^.targetTexture) <+>
                                               stepDirection =: V2 1 0                   <+>
                                               gaussWidth    =: realToFrac gwidth
                        & shaderTextures <<+>~ textureSampler =: toSample
        yData       = ShaderData RNil RNil
                        & shaderUniforms <<+>~ textureSizeField (yTarget^.targetTexture) <+>
                                               stepDirection =: V2 0 1                   <+>
                                               gaussWidth    =: realToFrac gwidth
                        & shaderTextures <<+>~ textureSampler =: (xTarget^.targetTexture)

    in do
        runRenderPass xPass xData $ singleton targetQuad
        runRenderPass yPass yData $ singleton targetQuad
        return $ yTarget^.targetTexture


gaussWidth :: SField YGaussWidth
gaussWidth = SField

