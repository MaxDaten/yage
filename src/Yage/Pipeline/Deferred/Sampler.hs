{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE KindSignatures   #-}
module Yage.Pipeline.Deferred.Sampler where

import Yage.Prelude
import Yage.Lens

import Yage.Viewport
import Yage.Geometry
import Yage.Uniforms as U hiding (ShaderSource)
import Yage.Rendering hiding (ShaderSource)

import Yage.TH.Shader
import Yage.Pipeline.Deferred.Common

type YNumSamples = "N_SAMPLES" ::: GLint
type YSampleWeights s = (s::Symbol) ::: [V1 GLfloat]

numSamples :: SField YNumSamples
numSamples = SField

sampleWeights :: KnownSymbol s => SField (YSampleWeights s)
sampleWeights = SField

type SingleSamplerData size tex = ShaderData [YProjectionMatrix, YTextureSize size] '[ TextureSampler tex ]

type SamplerData size tex = ShaderData '[ YTextureSize size ] '[ TextureSampler tex ]
type SamplerShader u t = Shader u t TargetVertex
type YageTextureSampler mrt u t = YageDeferredPass mrt (SamplerShader u t)


samplerPass :: String -> RenderTarget mrt -> Rectangle Int -> ShaderSource FragmentShader -> YageTextureSampler mrt u t
samplerPass debugName target targetRectangle fragSampler =
    let shaderRes   = ShaderProgramUnit
                        { _shaderName       = "Sampler.hs: " ++ debugName
                        , _shaderSources    = [ samplerVert^.shaderSource
                                              , fragSampler^.shaderSource
                                              ]
                        }
        samplerVert :: ShaderSource VertexShader
        samplerVert = $(vertexFile "res/glsl/pass/Sampling.vert")
    in passPreset target targetRectangle $ ShaderUnit shaderRes


sampleData :: ( KnownSymbol size, KnownSymbol sampler ) => Texture -> SamplerData size sampler
sampleData toSample =
    ShaderData (textureSizeField toSample) (SField =: toSample)


targetRectangleData :: Rectangle Int -> ShaderData '[ YProjectionMatrix ] '[]
targetRectangleData targetRectangle =
    let Rectangle xy0 xy1   = fromIntegral <$> targetRectangle
        uniforms            = projectionMatrix =: orthographicMatrix (xy0^._x) (xy1^._x) (xy1^._y) (xy0^._y) 0.0 1.0
    in ShaderData uniforms RNil


singleTextureSampler :: (KnownSymbol size, KnownSymbol tex) =>
                       Rectangle Int
                    -> Texture
                    -> SingleSamplerData size tex
singleTextureSampler target toSample = targetRectangleData target `append` sampleData toSample
