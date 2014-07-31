{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Yage.Pipeline.Deferred.GuiPass where

import Yage.Prelude
import Yage.Lens

import Yage.Uniforms.Material
import Yage.Uniforms.MVP
import Yage.Geometry            as Geometry
import Yage.Viewport
import Yage.Scene               hiding (toRenderEntity)
import Yage.Material
import Yage.Font
import Yage.Camera
import Yage.TH.Shader


import Yage.UI.GUI

import Yage.Rendering

import Yage.Pipeline.Deferred.Common

import qualified Graphics.Rendering.OpenGL as GL


type GUIFrameUniforms  = '[ YVPMatrix ]
-- shader data for each element in the gui
type GUIElementTexture = '[ YMaterialTex "ElementTexture" ]
type GUIElementUniform = '[ YModelMatrix ]

-- sum data for element and frame
type GUITextures = GUIElementTexture
type GUIUniforms = GUIFrameUniforms ++ GUIElementUniform
type GUIVertex   = Vertex (Y'P2TX2C4 GLfloat)

type GUIShader   = Shader GUIUniforms GUITextures GUIVertex

type GUIPass     = YageDeferredPass SingleRenderTarget GUIShader


-- | run
runGuiPass :: Texture -> YageRenderSystem GUI Texture
runGuiPass underlayTexture viewport gui = do
    passData `runPass` ( fmap toRenderEntity $ itoList $ gui^.guiElements )
    return $ target^.targetTexture

    where

    passData    :: ShaderData GUIFrameUniforms '[]
    passData    = orthographicUniforms viewport (gui^.guiCamera) `ShaderData` RNil

    texSpec     = mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    target      = mkSingleTargetFromSpec "YAGE.GUI" texSpec

    passDescr   :: GUIPass
    passDescr   = passPreset target (viewport^.rectangle) shader
                    & passPreRendering %~ flip (>>) glSettings

    runPass     = runRenderPass passDescr

    shader      :: GUIShader
    shader      = ShaderUnit $ ShaderProgramUnit
                        { _shaderName       = "GuiPass.hs"
                        , _shaderSources    = [ $(vertexFile   "res/glsl/pass/guiPass.vert")^.shaderSource
                                              , $(fragmentFile "res/glsl/pass/guiPass.frag")^.shaderSource
                                              ]
                        }

    glSettings = io $ do
        GL.clearColor    GL.$= GL.Color4 0 0 0 0
        GL.blendEquation GL.$= GL.FuncAdd
        GL.blendFunc     GL.$= (GL.One, GL.One)
        GL.blend         GL.$= GL.Enabled

        GL.depthFunc    GL.$= Nothing           -- disable func add
        GL.depthMask    GL.$= GL.Disabled       -- writing to depth is disabled
        --GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
        GL.clear [ GL.ColorBuffer ]

    -- | with a projection matrix for origin to the left bottom
    orthographicUniforms vp cam =
        let near              = realToFrac $ cam^.cameraZNear
            far               = realToFrac $ cam^.cameraZFar
            Rectangle xy0 xy1 = fromIntegral <$> vp^.viewportRect
            projM             = orthographicMatrix (xy0^._x) (xy1^._x) (xy0^._x) (xy1^._x) near far  :: M44 GLfloat
            viewM             = (fmap . fmap) realToFrac (cam^.cameraMatrix)                    :: M44 GLfloat
            vpM               = projM !*! viewM
        in vpMatrix         =: vpM


toRenderEntity :: (ByteString, GUIElement) -> RenderEntity GUIVertex (ShaderData GUIElementUniform GUIElementTexture)
toRenderEntity (ident, guiElement) = go guiElement & entMesh.meshId .~ ident
    where
    go (GUIFont buffer transformation) =
        RenderEntity ( buffer^.tbufMesh )
                     ( textGUIData buffer transformation )
                     ( GLDrawSettings GL.Triangles (Just GL.Back) )
    go guiElem = error $ "unsupported GUIElement: " ++ show guiElem


textGUIData :: TextBuffer -> Transformation Float -> ShaderData GUIElementUniform GUIElementTexture
textGUIData textBuffer transformation =
    let uniforms = modelMatrix =: (fmap realToFrac <$> transformation^.transformationMatrix)
        textures = SField =: (textBuffer^.tbufTexture.fontMap)
    in ShaderData uniforms textures


instance Implicit (FieldNames GUIElementTexture) where
    implicitly =
        SField =: "ElementTexture"
