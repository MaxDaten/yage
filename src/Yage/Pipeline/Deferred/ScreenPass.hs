{-# LANGUAGE OverloadedStrings #-}
module Yage.Pipeline.Deferred.ScreenPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry
import Yage.Geometry3D

import Yage.Scene
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.Rendering hiding (P3)

import qualified Graphics.Rendering.OpenGL as GL


newtype Screen = Screen (Viewport Int)

type SrcPerFrameUni    = '[ YProjectionMatrix ]
type SrcPerFrame       = ShaderData SrcPerFrameUni '[ YScreenTex ]

type SrcPerEntity      = ShaderData '[ YModelMatrix ] '[]

type ScrVertex         = Vertex (Y'P3TX2 GLfloat)

type ScreenPass        = PassDescr 
                            DefaultRenderTarget
                            SrcPerFrame
                            SrcPerEntity
                            ScrVertex


screenPass :: Texture -> Viewport Int -> ScreenPass
screenPass toScreen viewport = PassDescr
    { passTarget         = defaultRenderTarget
    , passShader         = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
    , passPerFrameData   = ShaderData screenUniforms screenTextures
    , passPreRendering   = io $ do
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
    , passPostRendering  = return ()
    }
    where

    glVP =  (realToFrac) <$> viewport

    screenUniforms :: Uniforms SrcPerFrameUni
    screenUniforms =
        projectionMatrix =: projectionMatrix2D 0 10 glVP

    screenTextures :: Textures '[YScreenTex]
    screenTextures = Field =: toScreen


toScrEntity :: Screen -> RenderEntity ScrVertex SrcPerEntity
toScrEntity (Screen vp)= RenderEntity screenMesh ( ShaderData uniforms mempty ) settings
    where
    uniforms =
        -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
        -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
        let dim           = realToFrac <$> vp^.viewportWH
            trans         = idTransformation & transPosition._xy .~ 0.5 * dim
                                             & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM
        in modelMatrix =: modelM

    screenMesh = mkFromVerticesF "YAGE:SCREEN" $ vertices . triangles $ addQuadTex $ quad 1

    -- TODO: not neccessary, can be moved to the shader (better for oculus sdk?)
    addQuadTex :: Primitive (Vertex (Y'P3 GLfloat)) -> Primitive ScrVertex
    addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                    (b <+> texture2 =: (V2 0 0))
                                                    (c <+> texture2 =: (V2 1 0))
                                                    (d <+> texture2 =: (V2 1 1))
    addQuadTex _ = error "not a quad"

    settings = GLDrawSettings GL.Triangles (Just GL.Back)

