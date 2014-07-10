{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.SkyPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Rendering                     hiding (P3)

import           Yage.Scene
import           Yage.Viewport
import           Yage.Uniforms
import           Yage.Material
import           Yage.HDR

import           Yage.Pipeline.Deferred.Common
import           Yage.Pipeline.Deferred.LightPass

import qualified Graphics.Rendering.OpenGL          as GL


type SkyPerFrame      = ShaderData PerspectiveUniforms '[]

type SkyUniforms      = '[ YModelMatrix ] ++ YSkyMaterial
type SkyTextures      = '[ YSkyTex ]
type SkyPerEntity     = ShaderData SkyUniforms SkyTextures

type SkyPass = PassDescr
                    LitPassChannels
                    SkyPerFrame
                    SkyPerEntity
                    LitVertex


type SkyMaterialRes = AResourceMaterial Cube
type SkyMaterial    = RenderMaterial

type SkyEntityT mesh mat = Entity (mesh LitVertex) mat
type SkyEntityRes   = SkyEntityT Mesh SkyMaterialRes
type SkyEntityDraw  = SkyEntityT Mesh SkyMaterial


skyPass :: LightPass -> Viewport Int -> Camera -> SkyPass
skyPass lighting viewport camera =
    let shaderRes = ShaderResource "res/glsl/pass/envPass.vert" "res/glsl/pass/envPass.frag"
        shaderData = ShaderData (perspectiveUniforms viewport camera) mempty
    in passPreset (lighting^.passTarget) (viewport^.rectangle) (shaderRes, shaderData)
        & passPreRendering .~ preRendering
    
    where
    
    preRendering   = io $ do
        GL.viewport     GL.$= viewport^.glViewport
        --GL.clearColor   GL.$= GL.Color4 0 0 0 0
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        -- we are looking from the inside into the sky box direction
        GL.frontFace    GL.$= GL.CW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)


toSkyEntity :: SkyEntityDraw -> RenderEntity LitVertex SkyPerEntity
toSkyEntity sky = toRenderEntity shData sky
    where
    shData   :: SkyPerEntity
    shData   = ShaderData uniforms RNil `append` material
    uniforms = modelMatrix =: ( sky^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )

    material :: YSkyData
    material = materialUniforms $ sky^.materials
