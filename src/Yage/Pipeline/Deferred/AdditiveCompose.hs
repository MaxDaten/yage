{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Yage.Pipeline.Deferred.AdditiveCompose where


import Yage.Prelude
import Yage.Lens

import Yage.Geometry as Geometry
import Yage.Uniforms as U
import Yage.Viewport
import Yage.Scene
import Yage.Material
import Yage.TH.Shader as GLSL

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type AddUniforms = [ YProjectionMatrix
                   , "BaseWeight" ::: GLfloat
                   , "AddWeight" ::: GLfloat
                   ]
type AddTextures = [ TextureUniform "TextureSampler0"
                   , TextureUniform "TextureSampler1"
                   ]
type AddFrameData = ShaderData AddUniforms AddTextures

type AdditiveShader = Shader (AddUniforms ++ '[YModelMatrix]) AddTextures TargetVertex
type AdditiveComposePass = YageDeferredPass SingleRenderTarget AdditiveShader

-------------------------------------------------------------------------------
-- | Fragment code
fragmentProgram :: GLSL.ShaderSource FragmentShader
fragmentProgram = [GLSL.yFragment|
#version 410 core

#include "pass/Sampling.frag"

uniform float BaseWeight = 1.0;
uniform float AddWeight = 1.0;

layout (location = 0) out vec3 pixelColor;

void main()
{
    vec3 texColor = BaseWeight * texture( TextureSampler0, SamplingUV0 ).rgb;
    texColor     += AddWeight  * texture( TextureSampler1, SamplingUV1 ).rgb;

    pixelColor = texColor;
}
|]
-------------------------------------------------------------------------------


-- | inplace additive
-- adds second argument texture to first argument
additiveCompose :: (Float, Texture) -> (Float, Texture) -> RenderSystem Texture
additiveCompose (baseWeight, baseTex) (addWeight, toAdd) =
    let target          = mkSingleTextureTarget $ baseTex & textureId <>~ ("+" ++ toAdd^.textureId)

        additiveDescr   :: AdditiveComposePass
        additiveDescr   = samplerPass "Yage.AdditiveCompose" target (target^.asRectangle) fragmentProgram

        frameData       :: AddFrameData
        frameData       = targetRectangleData (target^.asRectangle)
                            & shaderTextures <<+>~ (SField =: baseTex)
                            & shaderTextures <<+>~ (SField =: toAdd)
                            & shaderUniforms <<+>~ weights

        weights         = SField =: realToFrac baseWeight <+>
                          SField =: realToFrac addWeight

        additivePass    = runRenderPass additiveDescr
    in do
        frameData `additivePass` [ targetEntity baseTex ]
        return $ target^.targetTexture

instance Implicit ( FieldNames AddTextures ) where
    implicitly =
        SField =: "TextureSampler0" <+>
        SField =: "TextureSampler1"
