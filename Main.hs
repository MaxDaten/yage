{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Yage hiding ((</>))
import Yage.Wire hiding (unless, when)
import Yage.Lens
import Yage.Material
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
import Quine.GL.Texture hiding (Texture)
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
  throwWith "GL_FRAMEBUFFER_SRGB" $ glEnable GL_FRAMEBUFFER_SRGB
  throwWith "buildNamedStrings" $
    io (getDir "res/glsl") >>= \ ss -> buildNamedStrings ss ("/res/glsl"</>)
  -- trianglePass <- drawTriangle
  screenQuadPass <- drawToScreen
  return $ do
    game <- ask
    -- screenQuadPass . fmap (,game^.mainViewport) trianglePass
    screenQuadPass . return (game^.mainViewport)

-- * Draw Triangle

drawTriangle :: YageResource (RenderSystem IO Game (Texture PixelRGBA8))
drawTriangle = do
  vao <- glResource
  boundVertexArray $= vao

  -- ShaderPipeline
  pipeline <- [ $(embedShaderFile "res/glsl/pass-vertex.vert")
              , $(embedShaderFile "res/glsl/pass-color.frag")]
              `compileShaderPipeline` ["/res/glsl"]
  validatePipeline pipeline >>= \l -> unless (null l) $ print l

  Just vert <- get (vertexShader pipeline)
  Just frag <- get (fragmentShader pipeline)

  activeShaderProgram pipeline $= Just vert
  Just aPosition <- attributeLocation vert "aPosition"
  Just aColor    <- attributeLocation vert "aColor"

  -- Vertex & Index Buffer
  vbo <- glResource
  ebo <- glResource
  boundBufferAt ArrayBuffer $= vbo
  bufferData    ArrayBuffer $= (StaticDraw, [ (V3 (-1) 0 0, V3 1 0 0)
                                            , (V3 1 0 0   , V3 0 1 0)
                                            , (V3 0 1 0   , V3 0 0 1)
                                            , (V3 0 (-1) 0, V3 1 1 0)
                                            ] :: [(Vec3, Vec3)])
  boundBufferAt ElementArrayBuffer $= ebo
  bufferData    ElementArrayBuffer $= (StaticDraw, [0, 1, 2, 0, 3, 1] :: [Word8])

  setVertexAttribute aPosition $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (error "undefined access" :: Vec3)) nullPtr)
  setVertexAttribute aColor    $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (error "undefined access" :: Vec3)) (nullPtr `plusPtr` (sizeOf (error "undefined access" :: Vec3))))
  throwWith "Framebuffer" $ return ()

  -- Framebuffer
  targetTexture <- createTexture2D GL_TEXTURE_2D 1600 1200
  depthBuffer  <- createRenderbuffer 1600 1200 :: Acquire (Renderbuffer (DepthComponent24 Float))
  fb <- createFramebuffer [mkAttachment targetTexture] (Just $ mkAttachment depthBuffer) Nothing

  -- rendering
  return $ do
    boundFramebuffer RWFramebuffer $= fb
    vp <- view viewport
    glVp <- get Yage.glViewport
    when (vp^.rectangle /= glVp) $ do
      Yage.glViewport $= vp^.rectangle
      void $ resizeTexture2D targetTexture (vp^.rectangle.width) (vp^.rectangle.height)
      void $ resizeRenderbuffer depthBuffer (vp^.rectangle.width) (vp^.rectangle.height)

    glClearColor 1 0 1 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glEnable GL_DEPTH_TEST

    boundVertexArray $= vao
    currentProgram $= def
    boundProgramPipeline $= pipeline
    boundBufferAt ElementArrayBuffer $= ebo
    throwWith "drawing" $
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr

    return targetTexture

-- * Draw To Screen

drawToScreen :: YageResource (RenderSystem IO (Viewport Int) ())
drawToScreen = do
  emptyvao <- glResource
  boundVertexArray $= emptyvao

  pipeline <- [ $(embedShaderFile "res/glsl/screen.vert")
              , $(embedShaderFile "res/glsl/screen.frag")]
              `compileShaderPipeline` ["/res/glsl"]
  validatePipeline pipeline >>= \log -> unless (null log) $ error $ unlines log

  Right dynImg <- io (readImage "spectrum_gradient.png")
  dummyTex <- createTexture2DImage GL_TEXTURE_2D dynImg

  Just frag <- get (fragmentShader pipeline)
  iTexture    <- programUniform1i frag `liftM` uniformLocation frag "iTexture"
  iColor      <- programUniform4f frag `liftM` uniformLocation frag "iColor"
  iScreenSize <- programUniform4f frag `liftM` uniformLocation frag "iScreenSize"
  iColor   $= (1 :: Vec4)
  iTexture $= 6
  -- activeShaderProgram pipeline $= Just frag

  return $ do
    throwWith "fbo" $ do
      boundFramebuffer RWFramebuffer $= def

    vp <- ask
    glVp <- get Yage.glViewport
    when (vp^.rectangle /= glVp) $ do
      Yage.glViewport $= vp^.rectangle

    glClearColor 0 1 0 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glDisable GL_DEPTH_TEST
    boundVertexArray $= emptyvao
    currentProgram $= def

    boundProgramPipeline $= pipeline
    bindTexture GL_TEXTURE_2D iTexture $= Just dummyTex
    iScreenSize   $= vp^.screenSize

    throwWith "drawing" $
      glDrawArrays GL_TRIANGLES 0 3

 where
  screenSize = to $ \vp -> V4 (fromIntegral $ vp^.xy1._x) (fromIntegral $ vp^.xy1._y) (recip . fromIntegral $ vp^.xy2._x) (recip . fromIntegral $ vp^.xy2._y)

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
