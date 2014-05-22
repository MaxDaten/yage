{-# LANGUAGE OverloadedStrings #-}
module Yage.Pipeline.Deferred.SkyPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Rendering                     hiding (P3)

import           Yage.Scene
import           Yage.Uniforms
import           Yage.Material

import           Yage.Pipeline.Deferred.LightPass

import qualified Graphics.Rendering.OpenGL          as GL


type SkyPerFrame      = ShaderData PerspectiveUniforms '[]

type SkyUniforms      =  [ YModelMatrix, YSkyColor ]
type SkyTextures      = '[ YSkyTex ]
type SkyPerEntity     = ShaderData SkyUniforms SkyTextures

type SkyPass = PassDescr
                    LitPassChannels
                    SkyPerFrame
                    SkyPerEntity
                    LitVertex

type SkyEntity = Sky SkyMaterial
type SkyMaterial = RenderMaterial


skyPass :: LightPass -> ViewportI -> LitPassScene ent SkyMaterial -> SkyPass
skyPass lighting viewport scene = PassDescr
    { passTarget         = passTarget lighting
    , passShader         = ShaderResource "res/glsl/pass/envPass.vert" "res/glsl/pass/envPass.frag"
    , passPerFrameData   = ShaderData (perspectiveUniforms viewport (scene^.sceneCamera)) mempty
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


toSkyEntity :: SkyEntity -> RenderEntity LitVertex SkyPerEntity
toSkyEntity sky = toRenderEntity shData sky
    where
    shData   :: SkyPerEntity
    shData   = ShaderData uniforms RNil `append` material
    uniforms = modelMatrix =: ( (fmap . fmap) realToFrac $ calcModelMatrix $ sky^.transformation )

    material :: ShaderData '[ YSkyColor ] '[ YSkyTex ]
    material = materialUniforms $ sky^.materials
