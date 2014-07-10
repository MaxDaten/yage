{-# LANGUAGE FlexibleContexts           #-}
module Yage.Pipeline.Deferred.Common where

import Yage.Prelude
import Yage.Lens

import Yage.Viewport
import Yage.Geometry
import Yage.Transformation
import Yage.Uniforms as U
import Yage.Rendering
import Yage.Rendering.Textures

import qualified Yage.Core.OpenGL as GL

type YageDeferredPass mrt frameData = PassDescr mrt frameData 


{--
Pass Description
--}

passPreset :: RenderTarget target -> 
              Rectangle Int -> 
              (ShaderResource, frameData) -> 
              YageDeferredPass target frameData ent vert
passPreset target rect (shaderRes, frameData) = PassDescr
    { _passTarget         = target
    , _passShader         = shaderRes
    , _passPerFrameData   = frameData
    -- config
    , _passPreRendering   = io $ do
        GL.viewport     GL.$= (rect^.glViewport)
        GL.clearColor   GL.$= GL.Color4 0 0 0 0
        
        GL.depthFunc    GL.$= Just GL.Less
        GL.depthMask    GL.$= GL.Enabled
        
        GL.blend        GL.$= GL.Disabled

        GL.cullFace     GL.$= Just GL.Back
        GL.frontFace    GL.$= GL.CCW
        GL.polygonMode  GL.$= (GL.Fill, GL.Fill)

        -- GL.polygonMode  GL.$= (GL.Line, GL.Line)
        GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
    , _passPostRendering  = return ()
    }



deviceViewportPx :: Getter SingleRenderTarget (Viewport Int)
deviceViewportPx = to getter where
    getter target =
        let size = target^.textureSpec.texSpecDimension
        in Viewport (Rectangle 0 size) 2.2


type TargetVertex = Vertex (Y'P3TX2 GLfloat)
type TargetData   = ShaderData '[ YModelMatrix ] '[]
type TargetEntity = RenderEntity TargetVertex TargetData

targetEntity :: GetRectangle r Int => r -> TargetEntity
targetEntity hasRect =
    RenderEntity quadMesh ( ShaderData uniforms mempty ) settings
    where
    uniforms =
        -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
        -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable
        let dim          = realToFrac <$> hasRect^.asRectangle.extend
            trans        = idTransformation & transPosition._xy .~ 0.5 * dim
                                            & transScale        .~ V3 ( dim^._x ) (- (dim^._y) ) (1)
            scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
            modelM       = transM !*! scaleM
        in modelMatrix =: modelM

    quadMesh = mkFromVerticesF "YAGE:TARGETQUAD" . vertices . triangles $ targetFace

    targetFace :: Face TargetVertex
    targetFace = Face    
        (position3 =: V3 (-0.5) ( 0.5) 0.0 <+> texture2 =: (V2 0 1))
        (position3 =: V3 (-0.5) (-0.5) 0.0 <+> texture2 =: (V2 0 0))
        (position3 =: V3 ( 0.5) (-0.5) 0.0 <+> texture2 =: (V2 1 0))
        (position3 =: V3 ( 0.5) ( 0.5) 0.0 <+> texture2 =: (V2 1 1))

    settings = GLDrawSettings GL.Triangles (Just GL.Back)


-- | specs: GL.Float GL.RGB GL.RGB32F
mkSingleTargetHDR32 :: ByteString -> V2 Int -> RenderTarget SingleRenderTarget
mkSingleTargetHDR32 name size = RenderTarget (name ++ "-fbo") 
    $ SingleRenderTarget 
    $ mkTexture (name ++ "-buffer") $ TextureBuffer GL.Texture2D 
    $ mkTextureSpec size GL.Float GL.RGB GL.RGB32F
