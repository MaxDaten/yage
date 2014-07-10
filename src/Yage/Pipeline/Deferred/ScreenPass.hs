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

import Yage.Rendering hiding (P3)

import  Yage.Pipeline.Deferred.Common
import  Yage.Pipeline.Deferred.Sampler
import qualified Graphics.Rendering.OpenGL as GL


newtype Screen = Screen (Viewport Int)

type SrcPerFrameUni    = SamplerUniforms ++ [ YExposure, YExposureBias, YInverseGamma, YWhitePoint ]
type SrcPerFrame       = ShaderData SrcPerFrameUni '[ TextureUniform "ScreenTexture" ]

type SrcPerEntity      = ShaderData '[ YModelMatrix ] '[]

type ScrVertex         = Vertex (Y'P3TX2 GLfloat)

type ScreenPass        = YageDeferredPass 
                            DefaultRenderTarget
                            SrcPerFrame
                            SrcPerEntity
                            ScrVertex


screenPass :: Texture -> Viewport Int -> HDRCamera -> ScreenPass
screenPass toScreen viewport hdr = 
    let sampler = samplerPass toScreen defaultRenderTarget (viewport^.rectangle) "res/glsl/pass/screenPass.frag"
    in sampler
        & passPerFrameData.shaderUniforms <<+>~ screenUniforms
        & passPreRendering .~ preRender

    where

    screenUniforms :: Uniforms [ YExposure, YExposureBias, YInverseGamma, YWhitePoint ]
    screenUniforms =
        U.exposure         =: (realToFrac $ hdr^.hdrExposure)               <+>
        U.exposureBias     =: (realToFrac $ hdr^.hdrExposureBias)           <+>
        U.inverseGamma     =: (realToFrac $ recip(viewport^.viewportGamma)) <+>
        U.whitePoint       =: (realToFrac $ hdr^.hdrWhitePoint)


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

