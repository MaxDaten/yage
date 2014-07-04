module Yage.Pipeline.Deferred.DownsamplePass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry
import Yage.Geometry3D

import Yage.Scene
import Yage.Transformation
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.Rendering hiding (P3)
import Yage.Rendering.Textures (texSpecDimension)

import Yage.Pipeline.Deferred.Common

import qualified Graphics.Rendering.OpenGL as GL


type DSPerFrameUni    = '[ YProjectionMatrix, YTextureSize ]
type DSPerFrame       = ShaderData DSPerFrameUni '[ YDownsampleTex ]

type DownsamplePass    = YageDeferredPass 
                            SingleRenderTarget
                            DSPerFrame
                            TargetData
                            TargetVertex


downsamplePass :: Texture -> RenderTarget SingleRenderTarget -> DownsamplePass
downsamplePass toDownsample target =
    let shaderRes   = ShaderResource "res/glsl/pass/downsamplePass.vert" "res/glsl/pass/downsamplePass.frag"
        shaderData  = ShaderData sampleUniforms sampleTextures
    in passPreset target (target^.asRectangle) (shaderRes, shaderData)
    
    where

    sampleUniforms :: Uniforms DSPerFrameUni
    sampleUniforms =
        projectionMatrix =: projectionMatrix2D 0.0 1.0 (fromIntegral <$> target^.asRectangle) <+>
        textureSizeField toDownsample

    sampleTextures :: Textures '[ YDownsampleTex ]
    sampleTextures = 
        Field =: toDownsample



downsample :: Int -> Texture -> RenderSystem Texture
downsample downfactor toDownsample = 
    let name     = textureId toDownsample ++ (pack $ show downfactor)
        inSize   = texSpecDimension $ toDownsample^.textureSpec
        outSize  = floor <$> ((fromIntegral <$> inSize) ^/ fromIntegral downfactor)
        toTarget@(RenderTarget _ (SingleRenderTarget tex)) = mkSingleTargetHDR32 name outSize
    in do
        downsamplePass toDownsample toTarget `runRenderPass` [ targetEntity toTarget ]
        return tex
