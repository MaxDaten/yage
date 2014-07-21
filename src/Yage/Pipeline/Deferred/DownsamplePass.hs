{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yage.Pipeline.Deferred.DownsamplePass where

import Yage.Prelude
import Yage.Lens
import Data.ByteString.Lens

import Control.Applicative (liftA)
import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.TH.Shader

import Yage.Rendering
import Yage.Rendering.Textures (texSpecDimension)

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler

type DownsampleUniforms = [ YProjectionMatrix
                          , YTextureSize "TextureSize"
                          , YModelMatrix
                          ]
type DownsampleTextures = '[ TextureUniform "DownsampleTexture" ]
type DownsamplePass     = YageTextureSampler SingleRenderTarget DownsampleUniforms DownsampleTextures



downsampleBoxed5x5 :: Int -> Texture -> RenderSystem Texture
downsampleBoxed5x5 downfactor toDownsample = 
    let outSize  = liftA (`div` downfactor) $ toDownsample^.textureSpec.texSpecDimension
        target   = mkSingleTargetFromSpec ( toDownsample^.textureId ++ downfactor^.to show.packedChars )
                                          ( toDownsample^.textureSpec & texSpecDimension .~ outSize )
        
        fragment = $(fragmentFile "res/glsl/pass/boxedFilter5x5.frag")
        
        downsamplePass :: DownsamplePass
        downsamplePass = samplerPass "Yage.DownsamplePass" target (target^.asRectangle) fragment

        downsampleData :: ShaderData [ YProjectionMatrix, YTextureSize "TextureSize"] '[ TextureUniform "DownsampleTexture" ]
        downsampleData = targetRectangleData (target^.asRectangle) `append`
                         sampleData toDownsample 
    in do
        runRenderPass downsamplePass downsampleData [ targetEntity target ]
        return $ target^.targetTexture


instance Implicit (FieldNames '[ TextureUniform "DownsampleTexture" ]) where
    implicitly = SField =: "DownsampleTexture"