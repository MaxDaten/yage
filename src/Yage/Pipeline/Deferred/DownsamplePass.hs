module Yage.Pipeline.Deferred.DownsamplePass where

import Yage.Prelude
import Yage.Lens
import Data.ByteString.Lens
import Yage.Math


import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.Rendering hiding (P3)
import Yage.Rendering.Textures (texSpecDimension)

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type DownsamplePass    = YageDeferredPass 
                            SingleRenderTarget
                            (ShaderData SamplerUniforms '[ TextureUniform "DownsampleTexture" ])
                            TargetData
                            TargetVertex



downsample :: Int -> Texture -> RenderSystem Texture
downsample downfactor toDownsample = 
    let inSize   = toDownsample^.textureSpec.texSpecDimension
        outSize  = floor <$> ((fromIntegral <$> inSize) ^/ (fromIntegral downfactor :: Double))
        target   = mkSingleTargetFromSpec ( toDownsample^.textureId ++ downfactor^.to show.packedChars )
                                          ( toDownsample^.textureSpec & texSpecDimension .~ outSize )
        
        downsamplePass :: DownsamplePass
        downsamplePass = samplerPass toDownsample target (target^.asRectangle) "res/glsl/pass/boxedFilter5x5.frag"
    in do
        downsamplePass `runRenderPass` [ targetEntity target ]
        return $ target^.targetTexture
