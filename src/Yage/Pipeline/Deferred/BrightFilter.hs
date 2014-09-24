{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Yage.Pipeline.Deferred.BrightFilter where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry as Geometry
import Yage.Uniforms as U

import Yage.Viewport
import Yage.Scene
import Yage.Material
import Yage.TH.Shader

import Yage.Rendering

import Yage.Pipeline.Deferred.Common
import Yage.Pipeline.Deferred.Sampler


type BrightUniforms = [ YProjectionMatrix, YTextureSize "TextureSize0", YWhitePoint, YModelMatrix ]
type BrightTextures = '[ TextureUniform "TextureSampler0" ]

type BrightPass     = YageTextureSampler SingleRenderTarget BrightUniforms BrightTextures

brightFilter :: Texture -> Float -> RenderSystem Texture
brightFilter tex whitePoint =
    let target      = mkSingleTextureTarget $ tex & textureId <>~ "-brightPass"
        fragment    = $(fragmentFile "res/glsl/pass/brightFilter.frag")

        brightDescr :: BrightPass
        brightDescr = samplerPass "brightFilter" target (target^.asRectangle) fragment


        frameData   :: ShaderData [ YProjectionMatrix, YTextureSize "TextureSize0", YWhitePoint ] BrightTextures
        frameData   = singleTextureSampler (target^.asRectangle) tex
                        & shaderUniforms <<+>~ U.whitePoint =: realToFrac whitePoint

        brightPass  = runRenderPass brightDescr
    in do
        frameData `brightPass` [ targetEntity tex ]
        return $ target^.targetTexture




instance Implicit (FieldNames BrightTextures) where
    implicitly = SField =: "TextureSampler0"


