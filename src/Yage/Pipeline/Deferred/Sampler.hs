{-# LANGUAGE TemplateHaskell #-}
module Yage.Pipeline.Deferred.Sampler where

import Yage.Prelude


import Yage.Viewport
import Yage.Geometry
import Yage.Uniforms as U
import Yage.Rendering

import Yage.TH.Shader
import Yage.Pipeline.Deferred.Common

type SamplerUniforms = [ YProjectionMatrix, YTextureSize ]
type SamplerData t = ShaderData SamplerUniforms '[ TextureUniform t ]
type YageTextureSampler mrt t = YageDeferredPass mrt (SamplerData t) TargetData TargetVertex

samplerPass :: (KnownSymbol t) => String -> Texture -> RenderTarget mrt -> Rectangle Int -> ShaderSource -> YageTextureSampler mrt t
samplerPass debugName toSample target targetRectangle fragSampler =
    let shaderRes   = ShaderProgramUnit 
                        { _shaderName       = "Sampler.hs: " ++ debugName
                        , _shaderSources    = [samplerVert, fragSampler]
                        } 
    in passPreset target targetRectangle 
        $ ShaderUnit shaderRes 
            $ ShaderData sampleUniforms sampleTextures
    
    where

    sampleUniforms :: Uniforms [ YProjectionMatrix, YTextureSize ]
    sampleUniforms =
        projectionMatrix =: projectionMatrix2D 0.0 1.0 (fromIntegral <$> targetRectangle) <+>
        textureSizeField toSample

    sampleTextures :: (KnownSymbol t) => Textures '[ TextureUniform t ]
    sampleTextures = 
        SField =: toSample


samplerVert :: ShaderSource
samplerVert = $(vertexSrc "res/glsl/pass/renderToRect.vert")

