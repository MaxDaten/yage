{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE QuasiQuotes        #-}
-- | Renders all object parameters of a scene into the GBuffer.
module Yage.Rendering.Pipeline.Deferred.BaseGPass
  (
  ) where

import           Yage
import           Yage.Lens
import           Yage.Math
import           Yage.Prelude
import           Yage.GL

import           Data.Data
import           Foreign.Ptr

import           Yage.Camera
import qualified Yage.Formats.Ygm                        as YGM
import           Yage.Geometry                           as Geometry
import           Yage.Material
import           Yage.Scene
import           Yage.Uniforms                           as Uniforms
import           Yage.Viewport
import           Yage.HDR
import           Yage.Rendering.Resources.GL
import           Yage.Rendering.GL

import           Quine.GL.Types
import           Quine.GL.Uniform
import           Quine.GL.VertexArray
import           Quine.GL.Program
import           Quine.GL.Sampler
import           Quine.GL.Texture hiding (Texture)
import           Quine.GL.ProgramPipeline

import           Yage.Rendering.Pipeline.Deferred.Common

#include "definitions.h"
includePaths :: [FilePath]
includePaths = ["/res/glsl"]

type GBaseScene = Scene HDRCamera ({--ent--}) ({--env--}) ({--gui--})

-- | The output GBuffer of this pass
data GBuffer = GBuffer
  { _aBuffer     :: Texture PixelRGBA8
  , _bBuffer     :: Texture PixelRGBA8
  , _depthBuffer :: Texture (DepthComponent24 Float)
  } deriving (Typeable,Show)

-- * Draw To GBuffer

drawGBuffers :: YageResource (RenderSystem (GBaseScene,Viewport Int) GBuffer)
drawGBuffers = do
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- [ $(embedShaderFile "res/glsl/pass/base.vert")
              , $(embedShaderFile "res/glsl/pass/base.frag")]
              `compileShaderPipeline` includePaths

  Just frag <- get (fragmentShader $ pipeline^.pipelineProgram)
  Just vert <- get (vertexShader $ pipeline^.pipelineProgram)

  aChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  bChannel     <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  depthChannel <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture (DepthComponent24 Float)))
  fbo <- glResource

  lastViewportRef     <- newIORef (defaultViewport 1 1 :: Viewport Int)

  -- RenderPass
  return $ do
    (scene, mainViewport) <- ask
    lastViewport <- get lastViewportRef

    -- resizing the framebuffer
    when (mainViewport /= lastViewport) $ do
      Yage.glViewport    $= mainViewport^.rectangle
      lastViewportRef    $= mainViewport
      let V2 w h = mainViewport^.rectangle.extend
      forM_ [aChannel, bChannel] $ \ch -> do
        modifyM ch $ \x -> resizeTexture2D x w h
      colors <- sequence [get aChannel, get bChannel]
      modifyM depthChannel $ \x -> resizeTexture2D x w h
      depth  <- get depthChannel
      void $ attachFramebuffer fbo (mkAttachment <$> colors) (Just $ mkAttachment depth) Nothing

    boundFramebuffer RWFramebuffer $= fbo

    glClearColor 0 0 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glEnable GL_DEPTH_TEST
    glDisable GL_BLEND

    -- set globals
    currentProgram $= def
    boundProgramPipeline $= pipeline^.pipelineProgram
    -- set global shader uniforms

    -- element wise
    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= vao

    checkPipelineError pipeline

    {-# SCC glDrawElements #-} throwWithStack $
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr

    GBuffer <$> get aChannel <*> get bChannel <*> get depthChannel

