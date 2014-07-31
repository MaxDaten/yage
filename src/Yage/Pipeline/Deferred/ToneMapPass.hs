{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.ToneMapPass where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry

import Yage.Scene
import Yage.HDR
import Yage.Uniforms as U
import Yage.Viewport as VP
import Yage.TH.Shader

import Yage.Rendering
import Yage.Rendering.Textures

import  Yage.Pipeline.Deferred.Common
import  Yage.Pipeline.Deferred.Sampler
import qualified Graphics.Rendering.OpenGL as GL


newtype Tone = Tone (Viewport Int)

type ScrPerFrameUni    = [ YProjectionMatrix
                         , YTextureSize "TextureSize"
                         , YExposure
                         , YExposureBias
                         , YInverseGamma
                         , YWhitePoint
                         ]

type ScrVertex    = Vertex (Y'P3TX2 GLfloat)
type ToneUni    = ScrPerFrameUni ++ '[ YModelMatrix ]
type ToneTex    = '[ TextureUniform "ToneTexture" ]

type ToneShader = Shader ToneUni ToneTex ScrVertex

type TonePass   = YageDeferredPass SingleRenderTarget ToneShader


runToneMapPass :: Texture -> YageRenderSystem HDRCamera Texture
runToneMapPass texture viewport camera =
    let toneDescr = toneMapPass viewport
        tonePass  = runRenderPass toneDescr
        toneData  = toneMapData texture viewport camera
    in do
        toneData `tonePass` [ targetEntity $ viewport^.rectangle ]
        return $ toneDescr^.passTarget.targetTexture


toneMapPass :: Viewport Int -> TonePass
toneMapPass viewport =
    let fragment = $(fragmentFile "res/glsl/pass/toneMapPass.frag")
        sampler = samplerPass "Yage.ToneMap" target (viewport^.rectangle) fragment
    in sampler & passPreRendering .~ preRender

    where

    rgbSpec = mkTextureSpec' ( viewport^.rectangle.extend ) GL.RGB
    target  = mkSingleTextureTarget
                $ mkTargetTexture "Yage.ToneMap" rgbSpec

    preRender :: Renderer ()
    preRender = io $ do
        -- our 0/0 is top left (y-Axis is flipped)
        GL.viewport     GL.$= viewport^.glViewport
        GL.clearColor   GL.$= GL.Color4 1 1 1 0

        GL.depthFunc    GL.$= Nothing    -- TODO to init
        GL.depthMask    GL.$= GL.Disabled

        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)



toneMapData :: Texture -> Viewport Int -> HDRCamera -> ShaderData ScrPerFrameUni ToneTex
toneMapData texture viewport hdr =
    let shData = targetRectangleData (viewport^.rectangle) `append`
                 sampleData texture
    in shData & shaderUniforms <<+>~ toneUniforms

    where

    toneUniforms :: Uniforms [ YExposure, YExposureBias, YInverseGamma, YWhitePoint ]
    toneUniforms =
        U.exposure         =: (realToFrac $ hdr^.hdrExposure)               <+>
        U.exposureBias     =: (realToFrac $ hdr^.hdrExposureBias)           <+>
        U.inverseGamma     =: (realToFrac $ recip(viewport^.viewportGamma)) <+>
        U.whitePoint       =: (realToFrac $ hdr^.hdrWhitePoint)



instance (Implicit (FieldNames ToneTex)) where
    implicitly =
        SField =: "ToneTexture"

