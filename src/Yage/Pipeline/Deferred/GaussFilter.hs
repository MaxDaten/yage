{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Pipeline.Deferred.GaussFilter where

import Yage.Prelude
import Yage.Lens


import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP
import Yage.TH.Shader

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type GaussUniforms = [ YProjectionMatrix, YTextureSize "TextureSize", YModelMatrix ]
type GaussTextures = '[ TextureUniform "SamplingTexture"]
type GaussPass = YageTextureSampler SingleRenderTarget GaussUniforms GaussTextures


gaussFilter :: Texture -> RenderSystem Texture
gaussFilter toSample = 
    let targetX    = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussX" )
                                            ( toSample^.textureSpec )
        targetY    = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussY" )
                                            ( toSample^.textureSpec )
        
        xPass, yPass :: GaussPass
        xData, yData :: SingleSamplerData "TextureSize" "SamplingTexture"
        xPass = samplerPass "Yage.GaussX" targetX (targetX^.asRectangle) $(fragmentFile "res/glsl/pass/gaussFilterX.frag")
        xData = singleTextureSampler (targetX^.asRectangle) toSample

        yPass = samplerPass "Yage.GaussY" targetY (targetY^.asRectangle) $(fragmentFile "res/glsl/pass/gaussFilterY.frag")
        yData = singleTextureSampler (targetY^.asRectangle) toSample
    in do
        runRenderPass xPass xData [ targetEntity targetX ]
        runRenderPass yPass yData [ targetEntity targetY ]
        return $ targetY^.targetTexture


instance Implicit (FieldNames '[ TextureUniform "SamplingTexture" ]) where
    implicitly = SField =: "SamplingTexture"