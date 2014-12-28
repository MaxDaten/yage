{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module Main where

import Yage hiding ((</>))
import Yage.Wire hiding (unless, when)
import Yage.Lens
import Yage.Material
import Yage.GL
import Yage.Rendering.Pipeline.Deferred.ScreenPass
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
  , _sceneRenderer :: RenderSystem Game ()
  }

makeLenses ''Game

sceneWire :: YageWire t () Game
sceneWire = proc () -> do
  pipeline <- acquireOnce simplePipeline -< ()
  returnA -< Game (defaultViewport 800 600) pipeline

simplePipeline :: YageResource (RenderSystem Game ())
simplePipeline = do
  -- Convert output linear RGB to SRGB
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $
    io (getDir "res/glsl") >>= \ ss -> buildNamedStrings ss ("/res/glsl"</>)

  trianglePass   <- drawTriangle
  screenQuadPass <- drawRectangle

  return $ do
    game <- ask
    screenQuadPass . fmap (\t -> ([(1,t)],game^.mainViewport)) trianglePass

-- * Draw Triangle

drawTriangle :: YageResource (RenderSystem Game (Texture PixelRGBA8))
drawTriangle = do
  vao <- glResource
  boundVertexArray $= vao

  -- ShaderPipeline
  pipeline <- [ $(embedShaderFile "res/glsl/pass-vertex.vert")
              , $(embedShaderFile "res/glsl/pass-color.frag")]
              `compileShaderPipeline` ["/res/glsl"]
  validatePipeline pipeline >>= \l -> unless (null l) $ print l

  Just vert <- get (vertexShader $ pipeline^.pipelineProgram)
  Just frag <- get (fragmentShader $ pipeline^.pipelineProgram)

  activeShaderProgram (pipeline^.pipelineProgram) $= Just vert
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

  colorTex  <- createTexture2D GL_TEXTURE_2D 256 256 :: YageResource (Texture PixelRGBA8)
  depthBuff <- createRenderbuffer 256 256 :: YageResource RenderbufferD24F
  fbo       <- acquireFramebuffer [mkAttachment <$> readSlotResource (colorTex^.textureGL)] (Just $ pure $ mkAttachment $ depthBuff^.renderbufferGL) Nothing
  lastViewportRef     <- newIORef (defaultViewport 0 0)

  -- RenderPass
  return $ do
    traceM "drawTriangle"
    mainViewport <- view viewport
    lastViewport <- get lastViewportRef
    when (mainViewport /= lastViewport) $ do
      Yage.glViewport    $= mainViewport^.rectangle
      lastViewportRef    $= mainViewport
      let V2 w h = mainViewport^.rectangle.extend
      c <- get . view textureGL =<< resizeTexture2D colorTex w h
      void $ resizeRenderbuffer depthBuff w h
      void $ attachFramebuffer fbo [mkAttachment c] (Just $ mkAttachment depthBuff) Nothing

    traceM "boundFramebuffer"
    {-# SCC boundFramebuffer #-} throwWithStack $
      boundFramebuffer RWFramebuffer $= fbo
    traceM "end"

    glClearColor 1 0 1 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glEnable GL_DEPTH_TEST

    traceM "boundVertexArray"
    {-# SCC boundVertexArray #-} throwWithStack $
      boundVertexArray $= vao
    currentProgram $= def
    boundProgramPipeline $= pipeline^.pipelineProgram
    boundBufferAt ElementArrayBuffer $= ebo
    traceM "glDrawElements"
    {-# SCC glDrawElements #-} throwWithStack $
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr

    return colorTex

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

instance HasRenderSystem Game (ResourceT IO) Game () where
  renderSystem = sceneRenderer

instance (Storable a, Storable b) => Storable (a,b) where
  sizeOf _ = sizeOf (undefined::a) + sizeOf (undefined::b)
  alignment _ = max (alignment (undefined::a)) (alignment (undefined::b))
  peek ptr = (,) <$> peek (castPtr ptr) <*> peek (castPtr $ ptr `plusPtr` sizeOf (undefined::a))
  poke ptr (a,b) = poke (castPtr ptr) a >> poke (ptr `plusPtr` sizeOf (undefined::a)) b
