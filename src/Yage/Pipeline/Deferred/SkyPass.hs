{-# LANGUAGE OverloadedStrings #-}
module Yage.Pipeline.Deferred.SkyPass where

import Yage.Prelude
import Yage.Lens

import Yage.Rendering hiding (P3)

import Yage.Scene
import Yage.Uniforms

import Yage.Pipeline.Deferred.LightPass

import qualified Graphics.Rendering.OpenGL as GL


type SkyUniforms      = [ YModelMatrix, YIntensity, YSkyTexture ]

type SkyPass = PassDescr String LightPassChannels Sky PerspectiveUniforms SkyUniforms


skyPass :: LightPass -> ViewportI -> SScene geo mat LitVertex -> SkyPass
skyPass lighting viewport scene = PassDescr
    { passTarget         = passTarget lighting
    , passShader         = ShaderResource "res/glsl/pass/envPass.vert" "res/glsl/pass/envPass.frag"
    , passGlobalUniforms = perspectiveUniforms viewport (scene^.sceneCamera)
    , passEntityUniforms = skyUniforms
    , passGlobalTextures = []
    , passPreRendering   = io $ do
        GL.viewport     GL.$= toGLViewport viewport
        --GL.clearColor   GL.$= GL.Color4 0 0 0 0
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        -- we are looking from the inside into the sky box direction
        GL.frontFace    GL.$= GL.CW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

        --GL.polygonMode  GL.$= (GL.Line, GL.Line)
        --GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
        --GL.depthFunc    GL.$= Nothing
        --GL.depthMask    GL.$= GL.Disabled
    , passPostRendering  = return ()
    }

    where

    skyUniforms :: Sky -> Uniforms SkyUniforms
    skyUniforms sky = 
        let modelM = (fmap . fmap) realToFrac $ calcModelMatrix (sky^.skyTransformation) 
        in modelMatrix       =: modelM <+>
           intensity         =: (realToFrac $ sky^.skyIntensity) <+>
           skyTex            =: 0
