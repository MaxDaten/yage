{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Yage.Rendering.Pipeline.Deferred.ScreenPass
  ( drawRectangle
  ) where


import Yage hiding ((</>), toList)
import Yage.Lens
import qualified Data.Vector as V
import Linear.V
import Yage.GL
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Yage.Rendering.Resources.GL
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.Texture hiding (Texture)
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL

#include "definitions.h"

includePaths :: [FilePath]
includePaths = ["/res/glsl"]

-- * Draw To Screen

drawRectangle :: YageResource (RenderSystem ([(Vec4,Sampler,Texture PixelRGBA8)], Viewport Int) ())
drawRectangle = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/alphaBlendTextures.frag")]
              `compileShaderPipeline` includePaths
  validatePipeline pipeline >>= \logg -> unless (null logg) $ error $ unlines logg

  Just frag <- get (fragmentShader $ pipeline^.pipelineProgram)
  iTextures    <- textureUniforms frag "iTextures"
  iColors      <- colorUniforms frag "iColors"
  iUsedTex     <- programUniform programUniform1i frag "iUsedTextures"
  iTextures    $= (fromIntegral <$> textureUnits)

  lastViewportRef     <- newIORef (defaultViewport 0 0 :: Viewport Int)

  -- RenderPass
  return $ do
    throwWithStack $
      boundFramebuffer RWFramebuffer $= def

    ((colors, sampler, texs), mainViewport) <- over _1 unzip3 <$> ask
    lastViewport <- get lastViewportRef
    when (lastViewport /= mainViewport) $ do
      Yage.glViewport $= mainViewport^.rectangle
      lastViewportRef $= mainViewport

    glClearColor 0 1 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND

    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= emptyvao

    -- set shader uniforms
    currentProgram $= def
    boundProgramPipeline $= pipeline^.pipelineProgram

    throwWithStack $ do
      iColors   $= mkColorVector colors
      iUsedTex  $= fromIntegral (length texs)
      bindTextureSamplers GL_TEXTURE_2D $ zip (toList textureUnits) (Just <$> zip sampler texs)

    checkPipelineError pipeline

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3


textureUnits :: V MAX_TEXTURES TextureUnit
textureUnits = fromJust $ fromVector $ V.fromList [0 .. MAX_TEXTURES - 1]

-- | Creates the colors for each layer. The 'V' vector is filled to match the length of 'MAX_TEXTURE' with black.
mkColorVector :: [Vec4] -> V MAX_TEXTURES Vec4
mkColorVector cs = fromJust $ fromVector $ V.concat [fromList cs, V.replicate (MAX_TEXTURES - length cs) 0]

textureUniforms :: MonadIO m => Program -> String -> m (StateVar (V MAX_TEXTURES Int32))
textureUniforms = programUniform programUniform1iv

colorUniforms :: MonadIO m => Program -> String -> m (StateVar (V MAX_TEXTURES Vec4))
colorUniforms = programUniform programUniform4fv

 -- where
 --  screenSize = to $ \vp -> V4
 --    (fromIntegral $ vp^.xy1._x)
 --    (fromIntegral $ vp^.xy1._y)
 --    (recip . fromIntegral $ vp^.xy2._x)
 --    (recip . fromIntegral $ vp^.xy2._y)
