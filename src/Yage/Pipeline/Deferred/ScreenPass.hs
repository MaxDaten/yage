{-# LANGUAGE OverloadedStrings #-}
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
import qualified Graphics.Rendering.OpenGL as GL


newtype Screen = Screen (Viewport Int)

type SrcPerFrameUni    = '[ YProjectionMatrix, YExposure, YExposureBias, YInverseGamma ]
type SrcPerFrame       = ShaderData SrcPerFrameUni [ YScreenTex, YAddTex ]

type SrcPerEntity      = ShaderData '[ YModelMatrix ] '[]

type ScrVertex         = Vertex (Y'P3TX2 GLfloat)

type ScreenPass        = YageDeferredPass 
                            DefaultRenderTarget
                            SrcPerFrame
                            SrcPerEntity
                            ScrVertex


screenPass :: Texture -> Texture -> Viewport Int -> HDRCamera -> ScreenPass
screenPass toScreen toAdd viewport hdr = 
    let shaderRes   = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
        shaderData  = ShaderData screenUniforms screenTextures
    in (passPreset defaultRenderTarget (viewport^.rectangle) (shaderRes, shaderData))
        { passPreRendering = preRender }

    where

    screenUniforms :: Uniforms SrcPerFrameUni
    screenUniforms =
        projectionMatrix   =: projectionMatrix2D 0 10 (fromIntegral <$> viewport^.rectangle)   <+>
        U.exposure         =: (realToFrac $ hdr^.hdrExposure)           <+>
        U.exposureBias     =: (realToFrac $ hdr^.hdrExposureBias)       <+>
        U.inverseGamma     =: (realToFrac $ recip(viewport^.viewportGamma))

    screenTextures :: Textures [ YScreenTex, YAddTex ]
    screenTextures = 
        Field =: toScreen <+>
        Field =: toAdd

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
        
        GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]

