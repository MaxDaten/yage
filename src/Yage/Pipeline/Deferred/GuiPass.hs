{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE NamedFieldPuns     #-}
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

import qualified Yage.Core.OpenGL as GL

type YGUIElementType = "GUIType" ::: GUIElementType

type GUIFrameUniforms  = '[ YVPMatrix ]
-- shader data for each element in the gui
type GUIElementTexture = '[ YMaterialTex "ElementTexture" ]
type GUIElementUniform = '[ YModelMatrix, YGUIElementType ]

-- sum data for element and frame
type GUITextures = GUIElementTexture
type GUIUniforms = GUIFrameUniforms ++ GUIElementUniform
--type GUIVertex   = Vertex (Y'P2TX2C4 GLfloat)

type GUIShader   = Shader GUIUniforms GUITextures GUIVertex

type GUIPass     = YageDeferredPass GUIChannels GUIShader


data GUIChannels = GUIChannels
    { guiColor :: Texture
    , guiDepth :: Renderbuffer
    }

-- | run
runGuiPass :: Texture -> YageRenderSystem GUI Texture
runGuiPass underlayTexture viewport gui = do
    passData `runPass` ( fmap toRenderEntity $ itoList $ gui^.guiElements )
    return $ colorTex

    where

    passData    :: ShaderData GUIFrameUniforms '[]
    passData    = orthographicUniforms (gui^.guiCamera) `ShaderData` RNil

    texSpec     = mkTextureSpec' (viewport^.rectangle.extend) GL.RGBA
    colorTex    = mkTexture "YAGE.GUI.COLOR" $ TextureBuffer GL.Texture2D texSpec
    target      = RenderTarget "YAGE.GUI" $ GUIChannels
                    { guiColor = colorTex
                    , guiDepth = Renderbuffer "YAGE.GUI.DEPTH" $ mkTextureSpec' (viewport^.rectangle.extend) GL.DepthComponent
                    }

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

        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]

    -- | with a projection matrix for origin to the left bottom
    orthographicUniforms cam =
        let near              = realToFrac $ cam^.cameraZNear
            far               = realToFrac $ cam^.cameraZFar
            Rectangle xy0 xy1 = fromIntegral <$> viewport^.viewportRect
            projM             = orthographicMatrix (xy0^._x) (xy1^._x) (xy0^._x) (xy1^._y) near far  :: M44 GLfloat
            viewM             = (fmap . fmap) realToFrac (cam^.cameraMatrix)                         :: M44 GLfloat
            vpM               = projM !*! viewM
        in vpMatrix         =: vpM


toRenderEntity :: (ByteString, GUIElement) -> RenderEntity GUIVertex (ShaderData GUIElementUniform GUIElementTexture)
toRenderEntity (ident, guiElement) = go guiElement & entMesh.meshId .~ ident
    where
    go (GUIFont buffer transformation) =
        let imgTex   = mkTextureImg TexY8 $ buffer^.tbufTexture.fontMap
            tex      = mkTexture (buffer^.tbufTexture.font.to fontName.packedChars) $ Texture2D imgTex

            uniforms = modelMatrix =: (fmap realToFrac <$> transformation^.transformationMatrix) <+>
                       guiType =: TXT
            textures = SField =: tex
            shData   = ShaderData uniforms textures
        in RenderEntity ( buffer^.tbufMesh )
                        ( shData )
                        ( GLDrawSettings GL.Triangles (Just GL.Back) )

    go (GUISDF (mesh, texture) transformation) =
        let uniforms = modelMatrix =: (fmap realToFrac <$> transformation^.transformationMatrix) <+>
                       guiType =: SDF
            textures = SField      =: texture
            shData   = ShaderData uniforms textures
        in RenderEntity ( mesh )
                        ( shData )
                        ( GLDrawSettings GL.Triangles (Just GL.Back) )

    go (GUIImage (mesh, texture) transformation) =
        let uniforms = modelMatrix =: (fmap realToFrac <$> transformation^.transformationMatrix) <+>
                       guiType =: IMG
            textures = SField      =: texture
            shData   = ShaderData uniforms textures
        in RenderEntity ( mesh )
                        ( shData )
                        ( GLDrawSettings GL.Triangles (Just GL.Back) )

guiType :: SField YGUIElementType
guiType = SField



instance FramebufferSpec GUIChannels RenderTargets where
    fboColors GUIChannels{guiColor} =
        [ Attachment (ColorAttachment 0) $ TextureTarget GL.Texture2D guiColor 0
        ]

    fboDepth GUIChannels{guiDepth} =
        Just $ Attachment DepthAttachment $ RenderbufferTarget guiDepth


instance Implicit (FieldNames GUIElementTexture) where
    implicitly =
        SField =: "ElementTexture"

instance GL.AsUniform GUIElementType where
    asUniform ty = GL.asUniform ((fromIntegral $ fromEnum ty) :: GL.GLint)

instance GL.HasVariableType GUIElementType where
    variableType _ = GL.Int'

