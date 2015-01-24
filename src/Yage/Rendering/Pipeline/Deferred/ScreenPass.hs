{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Yage.Rendering.Pipeline.Deferred.ScreenPass
  ( textureToScreen
  , drawRectangle
  ) where


import Yage.Prelude hiding (toList)

import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Linear.V
import qualified Data.Vector as V
import Quine.GL.Program
import Quine.GL.ProgramPipeline
import Quine.GL.Sampler
import Quine.GL.Texture hiding (Texture)
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Quine.StateVar
import Yage.GL
import Yage.Lens
import Yage.Math
import Yage.Uniform
import Yage.Rendering.GL
import Yage.Rendering.Pipeline.Deferred.Common
import Yage.Rendering.RenderSystem
import Yage.Rendering.Resources.GL
import Yage.Resource.YageResource
import Yage.Viewport as GL

#include "definitions.h"

-- * Draw To Screen

data FragmentShader = FragmentShader
  { iTextures   :: UniformVar (V MAX_TEXTURES TextureUnit)
  , iWeights    :: UniformVar (V MAX_TEXTURES Vec4)
  , iUsedTex    :: UniformVar Int
  , iTargetSize :: UniformVar (Viewport Int)
  }

data ScreenPassState = ScreenPassState
  { fragment      :: FragmentShader
  , lastViewport  :: Viewport Int
  , vao           :: VertexArray
  , pipeline      :: Pipeline
  }

textureToScreen :: YageResource (RenderSystem m (Texture px) ())
textureToScreen = undefined

drawRectangle :: (MonadReader v m, HasViewport v Int, MonadResourceBase m) => YageResource (RenderSystem m [(Vec4,Sampler,Texture px)] ())
drawRectangle = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  p <- [ $(embedShaderFile "res/glsl/sampling/drawRectangle.vert")
       , $(embedShaderFile "res/glsl/sampling/filter.frag")]
        `compileShaderPipeline` includePaths

  Just frag <- traverse fragemenUniforms =<< get (fragmentShader $ p^.pipelineProgram)
  iTextures frag $= textureUnits

  let initState = ScreenPassState
        { fragment      = frag
        , lastViewport  = defaultViewport 0 0 :: Viewport Int
        , vao           = emptyvao
        , pipeline      = p
        }
  return $ flip mkStatefulRenderPass initState $ \s -> screenPass s . unzip3

screenPass :: (MonadReader v m, HasViewport v Int, MonadResourceBase m) => ScreenPassState -> ([Vec4],[Sampler],[Texture px]) -> m ((), ScreenPassState)
screenPass st@ScreenPassState{..} (colors, sampler, texs) = do
  let FragmentShader{..} = fragment
  throwWithStack $
    boundFramebuffer RWFramebuffer $= def

  mainViewport <- view viewport
  when (lastViewport /= mainViewport) $ do
    GL.glViewport $= mainViewport^.rectangle

  glDepthMask GL_TRUE
  glDisable GL_DEPTH_TEST
  glDisable GL_BLEND
  glDisable GL_CULL_FACE
  glFrontFace GL_CCW
  -- clear not neccessary
  -- glClearColor 0 1 0 1
  -- glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

  {-# SCC boundVertexArray #-} throwWithStack $
    boundVertexArray $= vao

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
  return ((), st{lastViewport = mainViewport})


fragemenUniforms :: Program -> YageResource FragmentShader
fragemenUniforms frag = FragmentShader
  <$> fmap (SettableStateVar.($=)) (textureUniforms frag "iTextures")
  <*> fmap (SettableStateVar.($=)) (colorUniforms frag "iWeights")
  <*> fmap (contramap fromIntegral . SettableStateVar.($=)) (programUniform programUniform1i frag "iUsedTextures")
  <*> fmap (contramap viewportToV4 . SettableStateVar.($=)) (programUniform programUniform4f frag "iTargetSize")

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