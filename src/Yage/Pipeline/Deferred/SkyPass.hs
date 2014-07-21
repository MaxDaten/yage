{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Yage.Pipeline.Deferred.SkyPass where

import           Yage.Prelude
import           Yage.Lens

import           Yage.Rendering

import           Yage.Scene
import           Yage.Viewport
import           Yage.Uniforms
import           Yage.Material
import           Yage.HDR
import           Yage.TH.Shader

import           Yage.Pipeline.Deferred.Common
import           Yage.Pipeline.Deferred.LightPass

import qualified Graphics.Rendering.OpenGL          as GL


type SkyEntityUni      = '[ YModelMatrix ] ++ YSkyMaterial


type SkyUni = PerspectiveUniforms ++ SkyEntityUni
type SkyTextures = '[ YSkyTex ]
type SkyShader = Shader SkyUni SkyTextures LitVertex
type SkyPass = PassDescr LitPassChannels SkyShader


type SkyMaterialRes = AResourceMaterial Cube
type SkyMaterial    = RenderMaterial

type SkyEntityT mesh mat = Entity (mesh LitVertex) mat
type SkyEntityRes   = SkyEntityT Mesh SkyMaterialRes
type SkyEntityDraw  = SkyEntityT Mesh SkyMaterial


skyPass :: LightPass -> Viewport Int -> SkyPass
skyPass lighting viewport =
    passPreset (lighting^.passTarget) (viewport^.rectangle) (ShaderUnit shaderProg)
        & passPreRendering .~ preRendering
    
    where

    shaderProg = ShaderProgramUnit
                    { _shaderName    = "SkyPass.hs"
                    , _shaderSources = [ $(vertexFile "res/glsl/pass/envPass.vert")^.shaderSource
                                       , $(fragmentFile "res/glsl/pass/envPass.frag")^.shaderSource
                                       ]
                    }


    
    preRendering   = io $ do
        GL.viewport     GL.$= viewport^.glViewport
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        -- we are looking from the inside into the sky box direction
        GL.frontFace    GL.$= GL.CW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)


skyFrameData :: Viewport Int -> Camera -> ShaderData PerspectiveUniforms '[]
skyFrameData viewport camera = ShaderData (perspectiveUniforms viewport camera) mempty


toSkyEntity :: SkyEntityDraw -> RenderEntity LitVertex (ShaderData SkyEntityUni SkyTextures)
toSkyEntity sky = toRenderEntity shData sky
    where
    shData   = ShaderData uniforms RNil `append` material
    uniforms = modelMatrix =: ( sky^.entityTransformation.transformationMatrix & traverse.traverse %~ realToFrac )

    material :: YSkyData
    material = materialUniforms $ sky^.materials


instance Implicit (FieldNames SkyTextures) where
    implicitly = SField =: "SkyTexture"
