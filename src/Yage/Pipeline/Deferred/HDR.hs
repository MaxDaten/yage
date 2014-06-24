module Yage.Pipeline.Deferred.HDR where

import Yage.Prelude
import Yage.Rendering
import Yage.Rendering.Textures
import qualified Yage.Core.OpenGL as GL

-- | specs: GL.Float GL.RGB GL.RGB32F
mkSingleTargetHDR :: String -> V2 Int -> RenderTarget SingleRenderTarget
mkSingleTargetHDR name size = RenderTarget (name ++ "-fbo") 
    $ SingleRenderTarget 
    $ mkTexture (pack name ++ "-buffer") $ TextureBuffer GL.Texture2D 
    $ mkTextureSpec size GL.Float GL.RGB GL.RGB32F


