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


type GaussPass = 
    YageDeferredPass 
        SingleRenderTarget
        (ShaderData SamplerUniforms '[ TextureUniform "SamplingTexture" ])
        TargetData
        TargetVertex


gaussFilter :: Texture -> RenderSystem Texture
gaussFilter toSample = 
    let targetX    = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussX" )
                                            ( toSample^.textureSpec )
        targetY    = mkSingleTargetFromSpec ( toSample^.textureId ++ "gaussY" )
                                            ( toSample^.textureSpec )
        
        gaussX :: GaussPass
        gaussX = samplerPass "Yage.GaussX" toSample targetX (targetX^.asRectangle) $(fragmentSrc "res/glsl/pass/gaussFilterX.frag")
        gaussY :: GaussPass
        gaussY = samplerPass "Yage.GaussY" (targetX^.targetTexture) targetY (targetY^.asRectangle) $(fragmentSrc "res/glsl/pass/gaussFilterY.frag")
    in do
        gaussX `runRenderPass` [ targetEntity targetX ]
        gaussY `runRenderPass` [ targetEntity targetY ]
        return $ targetY^.targetTexture


instance Implicit (FieldNames '[ TextureUniform "SamplingTexture" ]) where
    implicitly = SField =: "SamplingTexture"