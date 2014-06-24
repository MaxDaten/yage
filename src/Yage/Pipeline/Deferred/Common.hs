module Yage.Pipeline.Deferred.Common where

import Yage.Prelude
import Yage.Lens

import Yage.Viewport
import Yage.Rendering
import Yage.Rendering.Textures              (texSpecDimension)

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
    { passTarget         = target
    , passShader         = shaderRes
    , passPerFrameData   = frameData
    , passPreRendering   = io $ do
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
    , passPostRendering  = return ()
    }

deviceViewportPx :: Getter SingleRenderTarget (Viewport Int)
deviceViewportPx = to getter where
    getter target =
        let size = texSpecDimension $ target^.textureSpec
        in Viewport (Rectangle 0 size) 2.2
