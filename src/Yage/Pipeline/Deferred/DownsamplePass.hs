module Yage.Pipeline.Deferred.DownsamplePass where

import Yage.Prelude
import Yage.Lens
import Yage.Math

import Yage.Geometry
import Yage.Geometry3D

import Yage.Scene
import Yage.Transformation
import Yage.Uniforms as U
import Yage.Viewport as VP

import Yage.Rendering hiding (P3)

import Yage.Pipeline.Deferred.Common

import qualified Graphics.Rendering.OpenGL as GL


type DSPerFrameUni    = '[ YProjectionMatrix, YTextureSize ]
type DSPerFrame       = ShaderData DSPerFrameUni '[ YDownsampleTex ]

type DSPerEntity      = ShaderData '[ YModelMatrix ] '[]

type DSVertex         = Vertex (Y'P3TX2 GLfloat)

type DownsamplePass    = YageDeferredPass 
                            SingleRenderTarget
                            DSPerFrame
                            DSPerEntity
                            DSVertex


downsamplePass :: Texture -> RenderTarget SingleRenderTarget -> DownsamplePass
downsamplePass toDownsample target =
    let shaderRes   = ShaderResource "res/glsl/pass/downsamplePass.vert" "res/glsl/pass/downsamplePass.frag"
        shaderData  = ShaderData sampleUniforms sampleTextures
    in passPreset target (target^.asRectangle) (shaderRes, shaderData)
    
    where


    sampleUniforms :: Uniforms DSPerFrameUni
    sampleUniforms =
        projectionMatrix =: projectionMatrix2D 0.0 1.0 (fromIntegral <$> target^.asRectangle) <+>
        textureSizeField toDownsample


    sampleTextures :: Textures '[ YDownsampleTex ]
    sampleTextures = 
        Field =: toDownsample


quadTarget :: V2 Int -> V2 Int -> RenderEntity DSVertex DSPerEntity
quadTarget _ wh = RenderEntity quadMesh ( ShaderData uniforms mempty ) settings
    where
    uniforms =
        -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
        -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
        let dim           = realToFrac <$> wh
            trans         = idTransformation & transPosition._xy .~ 0.5 * dim
                                             & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM
        in modelMatrix =: modelM

    quadMesh = mkFromVerticesF "YAGE:RENDERTOQUAD" $ vertices . triangles $ addQuadTex $ quad 1

    -- TODO: not neccessary, can be moved to the shader (better for oculus sdk?)
    addQuadTex :: Primitive (Vertex (Y'P3 GLfloat)) -> Primitive DSVertex
    addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                    (b <+> texture2 =: (V2 0 0))
                                                    (c <+> texture2 =: (V2 1 0))
                                                    (d <+> texture2 =: (V2 1 1))
    addQuadTex _ = error "not a quad"

    settings = GLDrawSettings GL.Triangles (Just GL.Back)
