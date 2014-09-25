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


type GaussUniforms  = [ YProjectionMatrix, YStepDirection ]
type GaussTextures  = '[ TextureUniform "TextureSamplers[0]" ]

type GaussFrameData = ShaderData GaussUniforms GaussTextures
type GaussShader    = Shader ( GaussUniforms ++ '[ YModelMatrix ] ) GaussTextures TargetVertex
type GaussPass      = YageDeferredPass SingleRenderTarget GaussShader


-------------------------------------------------------------------------------
-- | Fragment code
gaussBlurFragmentProgram :: GLSL.ShaderSource FragmentShader
gaussBlurFragmentProgram = [GLSL.yFragment|
#version 410 core
// http://rastergrid.com/blog/2010/09/efficient-gaussian-blur-with-linear-sampling/

#include "pass/Sampling.frag"

uniform vec2 step;

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
    for (int i = 0; i < int(n); i++) {
        texColor += texture( TextureSamplers[0], SamplingUV[0] + offsets[i] * step);
    }
    pixelColor = texColor.rgb / n;
}
|]
-------------------------------------------------------------------------------

gaussFilter :: Texture -> RenderSystem Texture
gaussFilter toSample =
    let targetX     = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussX" )
                                             ( toSample^.textureSpec )
        targetY     = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussY" )
                                             ( toSample^.textureSpec )

        xPass, yPass :: GaussPass
        xPass       = samplerPass "Yage.GaussX" targetX (targetX^.asRectangle) gaussBlurFragmentProgram
        yPass       = samplerPass "Yage.GaussY" targetY (targetY^.asRectangle) gaussBlurFragmentProgram

        -- xData, yData :: SingleSamplerData "TextureSize0" "TextureSampler0"
        xData, yData :: GaussFrameData
        xData       = targetRectangleData (targetX^.asRectangle)
                        & shaderTextures <<+>~ SField =: toSample
                        & shaderUniforms <<+>~ stepDirection =: V2 1 0
        yData       = targetRectangleData (targetY^.asRectangle)
                        & shaderTextures <<+>~ SField =: (targetX^.targetTexture)
                        & shaderUniforms <<+>~ stepDirection =: V2 0 1

    in do
        runRenderPass xPass xData [ targetEntity targetX ]
        runRenderPass yPass yData [ targetEntity targetY ]
        return $ targetY^.targetTexture


instance Implicit (FieldNames GaussTextures) where
    implicitly = SField =: "TextureSamplers[0]"
