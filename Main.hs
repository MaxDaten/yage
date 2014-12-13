{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yage hiding ((</>))
import Yage.Wire
import Yage.Lens
import Yage.GL
import System.FilePath
import Yage.Rendering.Resources.GL
import Foreign.Ptr
import Foreign.Storable
import Data.FileEmbed
import Quine.Monitor
import Quine.GL
import Quine.GL.Attribute
import Quine.GL.Buffer
import Quine.GL.Error
import Quine.GL.Program
import Quine.GL.Shader
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.VertexArray
import Yage.Rendering.GL

appConf :: ApplicationConfig
appConf = defaultAppConfig{ logPriority = WARNING }

winSettings :: WindowConfig
winSettings = WindowConfig
  { windowSize = (800, 600)
  , windowHints =
    [ WindowHint'ContextVersionMajor  4
    , WindowHint'ContextVersionMinor  1
    , WindowHint'OpenGLProfile        OpenGLProfile'Core
    , WindowHint'OpenGLForwardCompat  True
    , WindowHint'OpenGLDebugContext   True
    , WindowHint'sRGBCapable          True
    , WindowHint'RefreshRate          60
    ]
  }

data Configuration = Configuration
  { _mainAppConfig      :: ApplicationConfig
  , _mainWindowConfig   :: WindowConfig
  , _mainMonitorOptions :: MonitorOptions
  }

makeLenses ''Configuration

configuration :: Configuration
configuration = Configuration appConf winSettings (MonitorOptions "localhost" 8080 True False)

data Game = Game
  { _mainViewport  :: Viewport Int
  , _sceneRenderer :: RenderSystem IO Game ()
  }

makeLenses ''Game

sceneWire :: YageWire t () Game
sceneWire = proc () -> do
  pipeline <- acquireOnce simplePipeline -< ()
  returnA -< Game (defaultViewport 800 600) pipeline

simplePipeline :: YageResource (RenderSystem IO Game ())
simplePipeline = do
  -- Convert output linear RGB to SRGB
  glEnable GL_FRAMEBUFFER_SRGB
  throwErrors
  print "buildNamedStrings"
  io (getDir "res/glsl") >>= \ ss -> buildNamedStrings ss ("/res/glsl"</>)
  throwErrors
  print "buildNamedStrings2"
  trianglePass <- drawTriangle
  return $ proc scene -> do
    trianglePass -< scene


drawTriangle :: YageResource (RenderSystem IO Game ())
drawTriangle = do
  glClearColor (1/57) (1/43) (1/67) 1

  throwErrors
  print "create shader program"
  transformVert <- compile GL_VERTEX_SHADER   "res/glsl/pass-vertex.vert"
  colorFrag     <- compile GL_FRAGMENT_SHADER "res/glsl/pass-color.frag"
  throwErrors
  print "link"
  prog <- link [transformVert,colorFrag]
  Just aPosition <- attributeLocation prog "aPosition"
  -- Just aColor    <- attributeLocation prog "aColor"
  throwErrors

  print "create buffer and buffer data"
  vao <- glResource
  vbo <- glResource
  ebo <- glResource
  boundVertexArray $= vao

  print "buffer data"
  boundBufferAt ArrayBuffer $= vbo
  bufferData    ArrayBuffer $= (StaticDraw, [V3 (-1) 0 0, V3 1 0 0, V3 0 1 0, V3 0 (-1) 0] :: [Vec3])
  boundBufferAt ElementArrayBuffer $= ebo
  bufferData    ElementArrayBuffer $= (StaticDraw, [0, 1, 2, 0, 3, 1] :: [Word8])
  throwErrors

  print "setup attributes"
  vertexAttribPointer 0 $ Layout 3 GL_FLOAT False (sizeOf (error "undefined access" :: Vec3)) nullPtr
  boundVertexArray $= def
  throwErrors

  -- rendering
  return $ do
    vp <- view viewport
    Yage.glViewport $= vp^.rectangle
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    currentProgram $= prog
    boundVertexArray $= vao
    boundBufferAt ElementArrayBuffer $= ebo
    throwErrors
    glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr
    throwErrors



main :: IO ()
main = yageMain "standalone" configuration sceneWire (1/60)

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasViewport Game Int where
  viewport = mainViewport

instance LinearInterpolatable Game where
  lerp _ _ = id

instance HasRenderSystem Game IO Game () where
  renderSystem = sceneRenderer
