{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yage.Rendering.Pipeline.Deferred.ScreenPass
  ( drawRectangle
  ) where


import Yage hiding ((</>))
import Yage.Lens
-- import Yage.Material
import Yage.GL
import Control.Monad.Reader (asks)
import Yage.Rendering.Resources.GL
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL


includePaths :: [FilePath]
includePaths = ["/res/glsl"]

-- * Draw To Screen

drawRectangle :: YageResource (RenderSystem (Texture PixelRGBA8, Viewport Int) ())
drawRectangle = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/alphaBlendTextures.frag")]
              `compileShaderPipeline` includePaths
  validatePipeline pipeline >>= \logg -> unless (null logg) $ error $ unlines logg

  Just frag <- get (fragmentShader $ pipeline^.pipelineProgram)
  iTextures    <- programUniform1i frag `liftM` uniformLocation frag "iTextures"
  iColors      <- programUniform4f frag `liftM` uniformLocation frag "iColors[0]"
  iColors   $= (1 :: Vec4)
  iTextures $= 6

  lastViewportRef     <- newIORef (defaultViewport 0 0 :: Viewport Int)

  -- RenderPass
  return $ do
    throwWith "fbo" $ do
      boundFramebuffer RWFramebuffer $= def

    (tex, vp) <- ask
    mainViewport <- asks snd
    lastViewport <- get lastViewportRef
    when (lastViewport /= mainViewport) $ do
      Yage.glViewport $= vp^.rectangle
      lastViewportRef $= mainViewport

    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND

    boundVertexArray $= emptyvao
    currentProgram $= def

    boundProgramPipeline $= (pipeline^.pipelineProgram)
    bindTexture GL_TEXTURE_2D iTextures (Just tex)

    throwWith "drawing" $
      glDrawArrays GL_TRIANGLES 0 3

 -- where
 --  screenSize = to $ \vp -> V4
 --    (fromIntegral $ vp^.xy1._x)
 --    (fromIntegral $ vp^.xy1._y)
 --    (recip . fromIntegral $ vp^.xy2._x)
 --    (recip . fromIntegral $ vp^.xy2._y)
