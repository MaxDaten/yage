module Yage.Pipeline.Deferred.Sampler where

import Yage.Prelude

import Yage.Viewport
import Yage.Geometry
import Yage.Uniforms as U
import Yage.Rendering

import Yage.Pipeline.Deferred.Common

type SamplerUniforms = [ YProjectionMatrix, YTextureSize ]
type SamplerData t = ShaderData SamplerUniforms '[ TextureUniform t ]
type YageTextureSampler mrt t = YageDeferredPass mrt (SamplerData t) TargetData TargetVertex

samplerPass :: (KnownSymbol t) => Texture -> RenderTarget mrt -> Rectangle Int -> FilePath -> YageTextureSampler mrt t
samplerPass toSample target targetRectangle sampler =
    let shaderRes   = ShaderResource "res/glsl/pass/renderToRect.vert" sampler
        shaderData  = ShaderData sampleUniforms sampleTextures
    in passPreset target targetRectangle (shaderRes, shaderData)
    
    where

    sampleUniforms :: Uniforms [ YProjectionMatrix, YTextureSize ]
    sampleUniforms =
        projectionMatrix =: projectionMatrix2D 0.0 1.0 (fromIntegral <$> targetRectangle) <+>
        textureSizeField toSample

    sampleTextures :: (KnownSymbol t) => Textures '[ TextureUniform t ]
    sampleTextures = 
        SField =: toSample
