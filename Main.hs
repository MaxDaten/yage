{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Yage hiding ((</>))
import Yage.Wire hiding (unless)
import Yage.Lens
import Yage.GL
import System.FilePath
import Yage.Rendering.Resources.GL
import Foreign.Ptr
import Foreign.Storable
import Data.FileEmbed
import qualified Data.ByteString.Char8 as Char8
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
import Quine.GL.ProgramPipeline
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
  throwWith "buildNamedStrings" $
    io (getDir "res/glsl") >>= \ ss -> buildNamedStrings ss ("/res/glsl"</>)
  trianglePass <- drawTriangle
  return $ proc scene -> do
    trianglePass -< scene


drawTriangle :: YageResource (RenderSystem IO Game ())
drawTriangle = do
  glClearColor (1/57) (1/43) (1/67) 1
  throwErrors
  vao <- glResource
  boundVertexArray $= vao

  pipeline <- compileShaderPipeline ["res/glsl/pass-vertex.vert", "res/glsl/pass-color.frag"] ["/res/glsl"]
  validatePipeline pipeline >>= \l -> unless (null l) $ print l
  Just vert <- get (vertexShader pipeline)
  Just frag <- get (fragmentShader pipeline)
  throwErrors

  activeShaderProgram pipeline $= Just vert
  print "attribs"
  Just aPosition <- attributeLocation vert "aPosition"
  Just aColor    <- attributeLocation vert "aColor"
  throwErrors

  print "create buffer and buffer data"
  vbo <- glResource
  ebo <- glResource

  print "buffer data"
  boundBufferAt ArrayBuffer $= vbo
  bufferData    ArrayBuffer $= (StaticDraw, [ (V3 (-1) 0 0, V3 1 0 0)
                                            , (V3 1 0 0   , V3 0 1 0)
                                            , (V3 0 1 0   , V3 0 0 1)
                                            , (V3 0 (-1) 0, V3 1 1 0)
                                            ] :: [(Vec3, Vec3)])
  boundBufferAt ElementArrayBuffer $= ebo
  bufferData    ElementArrayBuffer $= (StaticDraw, [0, 1, 2, 0, 3, 1] :: [Word8])
  throwErrors

  print "setup attributes"
  setVertexAttribute aPosition $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (error "undefined access" :: Vec3)) nullPtr)
  setVertexAttribute aColor    $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (error "undefined access" :: Vec3)) (nullPtr `plusPtr` (sizeOf (error "undefined access" :: Vec3))))
  boundVertexArray $= def
  throwErrors

  -- rendering
  return $ do
    vp <- view viewport
    Yage.glViewport $= vp^.rectangle
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    boundProgramPipeline $= pipeline
    boundVertexArray $= vao
    boundBufferAt ElementArrayBuffer $= ebo
    throwWith "drawing" $
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr


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

instance (Storable a, Storable b) => Storable (a,b) where
  sizeOf _ = sizeOf (undefined::a) + sizeOf (undefined::b)
  alignment _ = max (alignment (undefined::a)) (alignment (undefined::b))
  peek ptr = (,) <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOf (undefined::a))
  poke ptr (a,b) = poke (castPtr ptr) a >> poke (ptr `plusPtr` sizeOf (undefined::a)) b
