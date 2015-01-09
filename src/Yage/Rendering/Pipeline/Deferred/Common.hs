{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell  #-}

module Yage.Rendering.Pipeline.Deferred.Common
  ( embeddedShaders
  ) where

import System.FilePath
import Data.ByteString
import Data.FileEmbed

-- | Stored outside of 'Yage.Rendering.Pipeline.Deferred' in a seperated module
-- as a try to speed up the recompile times. A seperate module changes less often
-- than a core module
embeddedShaders :: [(FilePath,ByteString)]
embeddedShaders = $(embedDir "res/glsl")

-- import Yage.Prelude
-- import Yage.Lens

-- import Yage.Viewport
-- import Yage.Geometry
-- import Yage.Uniforms as U
-- import Yage.Rendering.RenderSystem
-- import Yage.Rendering.Textures

-- import qualified Yage.Core.OpenGL as GL

-- type YageDeferredPass mrt shader = PassDescr mrt shader


{--
Pass Description

passPreset :: RenderTarget target ->
              Rectangle Int ->
              Shader u t vert ->
              YageDeferredPass target (Shader u t vert)
passPreset target rect shader = PassDescr
    { _passTarget         = target
    , _passShader         = shader

    -- TODO : better config
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
        -- GL.clear        [ GL.ColorBuffer, GL.DepthBuffer ]
        -- GL.clear        [ GL.DepthBuffer ]
    , _passPostRendering  = return ()
    }




deviceViewportPx :: Getter SingleRenderTarget (Viewport Int)
deviceViewportPx = to getter where
    getter target =
        let size = target^.textureSpec.texSpecDimension
        in Viewport (Rectangle 0 size) 1 2.2


-- TODO simplify
type TargetVertex = Vertex (Y'P2 GLfloat)
type TargetData   = ShaderData '[] '[]
type TargetEntity = RenderEntity TargetVertex TargetData

targetQuad :: TargetEntity
targetQuad =
    RenderEntity quadMesh ( ShaderData mempty mempty ) settings
        -- our screen has it's origin (0/0) at the top left corner (y-Axis is flipped)
        -- we need to flip our screen object upside down with the object origin point at bottom left to keep the u/v coords reasonable

    where
    quadMesh = mkFromVerticesF "YAGE:TARGETQUAD" . vertices . triangles $ targetFace

    targetFace :: Face TargetVertex
    targetFace = Face
        (position2 =: V2 (-1) ( 1))
        (position2 =: V2 (-1) (-1))
        (position2 =: V2 ( 1) (-1))
        (position2 =: V2 ( 1) ( 1))

    settings = GLDrawSettings GL.Triangles (Just GL.Back)


-- | specs: GL.Float GL.RGB GL.RGB32F
-- see @mkTargetTexture
mkSingleTargetHDR32 :: ByteString -> V2 Int -> RenderTarget SingleRenderTarget
mkSingleTargetHDR32 name size = RenderTarget (name ++ "-fbo")
    $ SingleRenderTarget
    $ mkTargetTexture (name ++ "-buffer")
    $ mkTextureSpec size GL.Float GL.RGB GL.RGB32F


-- | creates a `Texture` for a RenderTarget with some recommended settings:
--
-- - no MipMap generation (expensive for targets every frame)
-- - linear mip/mag filter
-- - clamping to edge
-- - repetition repeated
mkTargetTexture :: ByteString -> BufferSpec -> Texture
mkTargetTexture name spec =
    let tex = mkTextureBuffer name GL.Texture2D spec
    in tex & textureConfig.texConfFiltering.texMipmapFilter  .~ Nothing
           & textureConfig.texConfFiltering.texMinFilter     .~ GL.Linear'
           & textureConfig.texConfFiltering.texMagFilter     .~ GL.Linear'
           & textureConfig.texConfWrapping.texWrapRepetition .~ GL.Mirrored
           & textureConfig.texConfWrapping.texWrapClamping   .~ GL.Clamp
--}
