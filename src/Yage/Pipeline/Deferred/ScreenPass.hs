{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.ScreenPass where

import Yage.Prelude
import Yage.Lens

import Yage.Geometry

import Yage.Scene
import Yage.HDR
import Yage.Uniforms as U
import Yage.Viewport as VP
import Yage.TH.Shader

import Yage.Rendering

import  Yage.Pipeline.Deferred.Common
import  Yage.Pipeline.Deferred.Sampler
import qualified Graphics.Rendering.OpenGL as GL


newtype Screen = Screen (Viewport Int)

type ScrPerFrameUni    = [ YProjectionMatrix
                         , YTextureSize "TextureSize"
                         , YExposure
                         , YExposureBias
                         , YInverseGamma
                         , YWhitePoint 
                         ]

type ScrVertex    = Vertex (Y'P3TX2 GLfloat)
type ScreenUni    = ScrPerFrameUni ++ '[ YModelMatrix ]
type ScreenTex    = '[ TextureUniform "ScreenTexture" ]

type ScreenShader = Shader ScreenUni ScreenTex ScrVertex

type ScreenPass   = YageDeferredPass DefaultRenderTarget ScreenShader


screenPass :: Viewport Int -> ScreenPass
screenPass viewport = 
    let fragment = $(fragmentFile "res/glsl/pass/screenPass.frag")
        sampler = samplerPass "Yage.ScreenPass" defaultRenderTarget (viewport^.rectangle) fragment
    in sampler & passPreRendering .~ preRender

    where
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



screenFrameData :: Texture -> Viewport Int -> HDRCamera -> ShaderData ScrPerFrameUni ScreenTex
screenFrameData texture viewport hdr =
    let shData = targetRectangleData (viewport^.rectangle) `append` 
                 sampleData texture
    in shData & shaderUniforms <<+>~ screenUniforms

    where
    
    screenUniforms :: Uniforms [ YExposure, YExposureBias, YInverseGamma, YWhitePoint ]
    screenUniforms =
        U.exposure         =: (realToFrac $ hdr^.hdrExposure)               <+>
        U.exposureBias     =: (realToFrac $ hdr^.hdrExposureBias)           <+>
        U.inverseGamma     =: (realToFrac $ recip(viewport^.viewportGamma)) <+>
        U.whitePoint       =: (realToFrac $ hdr^.hdrWhitePoint)




instance (Implicit (FieldNames '[TextureUniform "ScreenTexture"])) where
    implicitly = SField =: "ScreenTexture"
