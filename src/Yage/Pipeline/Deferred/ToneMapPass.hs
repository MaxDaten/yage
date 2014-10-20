{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.ToneMapPass where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry

import Yage.Scene
import Yage.HDR
import Yage.Uniforms as U
import Yage.Viewport as VP
import Yage.TH.Shader as GLSL

import Yage.Rendering
import Yage.Rendering.Textures

import  Yage.Pipeline.Deferred.Common
import  Yage.Pipeline.Deferred.Sampler
import qualified Graphics.Rendering.OpenGL as GL


newtype Tone = Tone (Viewport Int)

type ScrPerFrameUni    = [ YProjectionMatrix
                         , YExposure
                         , YExposureBias
                         , YInverseGamma
                         , YWhitePoint
                         , YNumSamples
                         , YSampleWeights "weights[0]"
                         ]

type ScrVertex    = Vertex (Y'P3TX2 GLfloat)
type ToneUni    = ScrPerFrameUni ++ '[ YModelMatrix ]
type ToneTex    = '[ TextureSampler "TextureSamplers[0]"
                   , TextureArray "TextureSamplers[1]"
                   ]

type ToneShader = Shader ToneUni ToneTex ScrVertex

type TonePass   = YageDeferredPass SingleRenderTarget ToneShader


-------------------------------------------------------------------------------
-- | Fragment code
toneFragmentProgramm :: GLSL.ShaderSource FragmentShader
toneFragmentProgramm = [GLSL.yFragment|
#version 410 core
// http://frictionalgames.blogspot.de/2012/09/tech-feature-hdr-lightning.html
// http://http.download.nvidia.com/developer/presentations/2004/6800_Leagues/6800_Leagues_HDR.pdf
// http://filmicgames.com/archives/75

#include "pass/Sampling.frag"

// 0: LinearToneMapping
// 1: ReinhardToneMapping
// 2: Uncharted2ToneMapping
#define TONE_MAPPING_TYPE 2

uniform float InverseGamma  = 1.0 / 2.2;
uniform float Exposure      = 1.0;
uniform float ExposureBias  = 1.0;
uniform float WhitePoint    = 11.2;

uniform float weights[ MAX_TEXTURES ];

layout (location = 0) out vec3 pixelColor;

//------------------------------------

vec4 ToneColor()
{
    vec4 OutColor = vec4(0);
    OutColor.rgb = texture( TextureSamplers[0], SamplingUV[0] ).rgb;

    for ( int i = 1; i < (N_SAMPLES + 1); i++ )
    {
        vec4 sampleColor = texture( TextureSamplers[i], SamplingUV[0] );
        OutColor.rgb  += weights[i-1] * sampleColor.rgb;
        OutColor.a    += sampleColor.a / N_SAMPLES;
    }
    return OutColor;
}

vec3 inverseGamma(vec3 x)
{
    return pow(x, vec3(InverseGamma));
}

vec3 LinearToneMapping(vec3 color)
{
    return color;
}

vec3 ReinhardToneMapping(vec3 color)
{
    color = color / (1+color);
    return color;
}

vec3 Uncharted2ToneMapping(vec3 color)
{
    float A = 0.15;
    float B = 0.50;
    float C = 0.10;
    float D = 0.20;
    float E = 0.02;
    float F = 0.30;
    return ((color*(A*color+C*B)+D*E) / (color*(A*color+B)+D*F))- E / F;
}

vec3 ToneMapping(vec3 color)
{
#if     TONE_MAPPING_TYPE == 0
    return LinearToneMapping(color);
#elif   TONE_MAPPING_TYPE == 1
    return ReinhardToneMapping(color);
#elif   TONE_MAPPING_TYPE == 2
    return Uncharted2ToneMapping(color);
#endif
}

void main()
{
    vec3 texColor = ToneColor().rgb;

    texColor     *= Exposure;

    vec3 color = 2.0 * ToneMapping( ExposureBias + texColor );
    vec3 whiteScale = 1.0 / ToneMapping(vec3(WhitePoint));


    color *= whiteScale;
    color = inverseGamma( color );
    pixelColor = clamp( color, 0, 1 );
}
|]

-- ============================================================================

runToneMapPass :: Texture -> [ (Float, Texture) ] -> YageRenderSystem HDRCamera Texture
runToneMapPass baseTexture samples viewport camera =
    let tonePass  = runRenderPass toneDescr
    in do
        toneData `tonePass` ( viewport^.rectangle.to targetEntity.to singleton )
        return $ toneDescr^.passTarget.targetTexture

    where

    toneDescr :: TonePass
    toneDescr =
        let sampler = samplerPass "Yage.ToneMap" target (viewport^.rectangle) toneFragmentProgramm
        in sampler & passPreRendering .~ preRender

        where

        rgbSpec = mkTextureSpec' ( viewport^.rectangle.extend ) GL.RGB
        target  = mkSingleTextureTarget
                    $ mkTargetTexture "Yage.ToneMap" rgbSpec

        preRender :: Renderer ()
        preRender = io $ do
            -- our 0/0 is top left (y-Axis is flipped)
            GL.viewport     GL.$= viewport^.glViewport
            GL.clearColor   GL.$= GL.Color4 1 1 1 0

            GL.depthFunc    GL.$= Nothing    -- TODO to init
            GL.depthMask    GL.$= GL.Disabled

            GL.blend        GL.$= GL.Disabled

            GL.cullFace     GL.$= Just GL.Back
            GL.frontFace    GL.$= GL.CCW
            GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

    toneData :: ShaderData ScrPerFrameUni ToneTex
    toneData = targetRectangleData (viewport^.rectangle)
                    & shaderTextures <<+>~ textureSampler =: baseTexture
                    & shaderTextures <<+>~ textureArray   =: map snd samples
                    & shaderUniforms <<+>~ toneUniforms

        where

        toneUniforms :: Uniforms [ YExposure, YExposureBias, YInverseGamma, YWhitePoint, YNumSamples, YSampleWeights "weights[0]" ]
        toneUniforms =
            U.exposure         =: (realToFrac $ camera^.hdrExposure)               <+>
            U.exposureBias     =: (realToFrac $ camera^.hdrExposureBias)           <+>
            U.inverseGamma     =: (realToFrac $ recip(viewport^.viewportGamma))    <+>
            U.whitePoint       =: (realToFrac $ camera^.hdrWhitePoint)             <+>
            numSamples         =: (fromIntegral $ 1 {-baseTexture-} + length samples) <+>
            sampleWeights      =: map (V1 . realToFrac . fst) samples
