{-# LANGUAGE OverloadedStrings #-}
module Yage.Pipeline.Deferred.ScreenPass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Scene
import Yage.Camera
import Yage.Geometry
import Yage.Primitives

import Yage.Rendering hiding (P3)
import Yage.Rendering.Transformation

import Yage.Pipeline.Deferred.Common

import qualified Graphics.Rendering.OpenGL as GL


newtype Screen = Screen ViewportI

type ScrGlobalUniforms = [YProjectionMatrix, YScreenTex]
type ScrLocalUniforms  = '[YModelMatrix]
type ScrVertex         = P3TX2

type ScreenPass = PassDescr String DefaultRenderTarget Screen ScrGlobalUniforms ScrLocalUniforms

screenPass :: Texture -> ViewportI -> ScreenPass
screenPass toScreen viewport = PassDescr
    { passTarget         = defaultRenderTarget
    , passShader         = ShaderResource "res/glsl/pass/screenPass.vert" "res/glsl/pass/screenPass.frag"
    , passGlobalUniforms = screenUniforms
    , passEntityUniforms = viewportUniforms
    , passGlobalTextures = [TextureDefinition (0, "screentex") toScreen]
    , passPreRendering   = io $ do
        -- our 0/0 is top left (y-Axis is flipped)
        GL.viewport     GL.$= toGLViewport viewport
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

    screenUniforms :: Uniforms ScrGlobalUniforms
    screenUniforms =
        let 
            projM = cameraProjectionMatrix (Camera2D fpsCamera $ CameraPlanes 0 10) glVP :: M44 GLfloat
        in projectionMatrix =: projM <+>
           screenTex        =: 0

    viewportUniforms :: Screen -> Uniforms ScrLocalUniforms
    viewportUniforms (Screen vp) =
        -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
        -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
        let dim           = realToFrac <$> vp^.vpSize
            trans         = idTransformation & transPosition._xy .~ 0.5 * dim
                                             & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
            --trans        = screenEnt^.transformation 
            --                    & transPosition._xy    +~ ((0.5) * screenEnt^.transformation.transScale._xy)
            --                    & transScale._y        *~ (-1)
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM
        in modelMatrix =: modelM





instance Renderable Screen ScrVertex where
    renderDefinition _ = 
        let q    = (vertices . triangles $ addQuadTex $ quad 1) :: [Vertex ScrVertex]
            mesh = makeSimpleTriMesh "YAGE:SCREEN" q
        in RenderEntity mesh (GLDrawSettings Triangles (Just Back)) []

addQuadTex :: Primitive (Vertex P3) -> Primitive (Vertex P3TX2)
addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                (b <+> texture2 =: (V2 0 0))
                                                (c <+> texture2 =: (V2 1 0))
                                                (d <+> texture2 =: (V2 1 1))
addQuadTex _ = error "not a quad"

