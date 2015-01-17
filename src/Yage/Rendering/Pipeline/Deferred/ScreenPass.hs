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
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.GL.Program
import Quine.GL.Sampler
import Quine.GL.Texture hiding (Texture)
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"

-- * Draw To Screen

drawRectangle :: YageResource (RenderSystem ([(Vec4,Sampler,Texture px)], Viewport Int) ())
drawRectangle = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
              , $(embedShaderFile "res/glsl/sampling/filter.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- get (fragmentShader $ pipeline^.pipelineProgram)
  iTextures    <- textureUniforms frag "iTextures"
  iWeights     <- colorUniforms frag "iWeights"
  iUsedTex     <- programUniform programUniform1i frag "iUsedTextures"
  iTargetSize  <- fmap (contramap viewportToV4 . SettableStateVar.($=)) $ programUniform programUniform4f frag "iTargetSize"
  iTextures    $= textureUnits

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

    glDepthMask GL_TRUE
    glDisable GL_DEPTH_TEST
    glDisable GL_BLEND
    glDisable GL_CULL_FACE
    glFrontFace GL_CCW
    -- clear not neccessary
    -- glClearColor 0 1 0 1
    -- glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= emptyvao

    -- set shader uniforms

    boundProgramPipeline $= pipeline^.pipelineProgram
    checkPipelineError pipeline

    throwWithStack $ do
      iWeights    $= mkColorVector colors
      iUsedTex    $= fromIntegral (length texs)
      iTargetSize $= mainViewport
      bindTextureSamplers GL_TEXTURE_2D $ zip (toList textureUnits) (Just <$> zip sampler texs)

    throwWithStack $
      glDrawArrays GL_TRIANGLES 0 3


viewportToV4 :: Viewport Int -> Vec4
viewportToV4 vp =
  let Rectangle _ wh = vp^.rectangle
  in V4 (fromIntegral $ wh^._x) (fromIntegral $ wh^._y) (recip $ fromIntegral $ wh^._x) (recip $ fromIntegral $ wh^._y)

textureUnits :: V MAX_TEXTURES TextureUnit
textureUnits = fromJust $ fromVector $ V.fromList [0 .. MAX_TEXTURES - 1]

-- | Creates the colors for each layer. The 'V' vector is filled to match the length of 'MAX_TEXTURE' with black.
mkColorVector :: [Vec4] -> V MAX_TEXTURES Vec4
mkColorVector cs = fromJust $ fromVector $ V.concat [fromList cs, V.replicate (MAX_TEXTURES - length cs) 0]

textureUniforms :: MonadIO m => Program -> String -> m (StateVar (V MAX_TEXTURES TextureUnit))
textureUniforms p str = mapStateVar (fmap fromIntegral) (fmap fromIntegral) `liftM` programUniform programUniform1iv p str

colorUniforms :: MonadIO m => Program -> String -> m (StateVar (V MAX_TEXTURES Vec4))
colorUniforms = programUniform programUniform4fv

 -- where
 --  screenSize = to $ \vp -> V4
 --    (fromIntegral $ vp^.xy1._x)
 --    (fromIntegral $ vp^.xy1._y)
 --    (recip . fromIntegral $ vp^.xy2._x)
 --    (recip . fromIntegral $ vp^.xy2._y)
