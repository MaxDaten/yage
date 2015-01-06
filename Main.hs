{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
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
import Yage.Scene
import Yage.HDR
import Yage.GL
import Yage.Rendering.Pipeline.Deferred.ScreenPass
import Yage.Rendering.Pipeline.Deferred.BaseGPass
import Yage.Rendering.RenderData
import Yage.Formats.Ygm
import Yage.Resources
import System.FilePath
import Yage.Rendering.Resources.GL
import Foreign.Ptr
import Foreign.Storable
import Data.FileEmbed
import Data.Data
import qualified Data.ByteString.Char8 as Char8
import Quine.Monitor
import Quine.GL
import Quine.GL.Attribute
import Quine.GL.Buffer
import Quine.GL.Error
import Quine.GL.Program
import Quine.GL.Shader
import Quine.GL.Sampler
import Quine.GL.Types
import Quine.GL.Uniform
import Quine.GL.Texture hiding (Texture)
import Quine.GL.VertexArray
import Quine.GL.ProgramPipeline
import Yage.Rendering.GL
import Graphics.GL.Ext.EXT.TextureFilterAnisotropic

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

data GVertex = GVertex
  { pos  :: !Vec3
  , tex  :: !Vec2
  , tanx :: !Vec3
  , tanz :: !Vec4
  } deriving (Show,Ord,Eq,Data,Typeable,Generic)

instance Storable GVertex where
  peek ptr =
    GVertex <$> peek (castPtr ptr)
            <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3)))
            <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)))
            <*> peek (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3)))
  poke ptr GVertex{..} = do
    poke (castPtr ptr) pos
    poke (castPtr $ ptr `plusPtr` sizeOf (undefined::Vec3)) tex
    poke (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2))) tanx
    poke (castPtr $ ptr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3))) tanz
  sizeOf _ = sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3) + sizeOf (undefined::Vec4)
  alignment _ = alignment (undefined::Vec3)


type GameEntity = Entity (RenderData (SVector Word32) (SVector GVertex)) GBaseMaterial

instance HasGBaseVertexLayout (SVector GVertex) where
  gBaseVertexLayout _ = GBaseVertexLayout
    { _vPosition = Layout 3 GL_FLOAT False stride (nullPtr)
    , _vTexture  = Layout 2 GL_FLOAT False stride (nullPtr `plusPtr` (sizeOf (undefined::Vec3)))
    , _vTangentX = Layout 3 GL_FLOAT False stride (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)))
    , _vTangentZ = Layout 4 GL_FLOAT False stride (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3)))
    }
    where
    stride = sizeOf (undefined::GVertex)

instance HasGBaseMaterial mat => HasGBaseMaterial (Entity d mat) where
  gBaseMaterial = materials.gBaseMaterial

instance HasRenderData (Entity (RenderData i v) mat) i v where
  renderData = Yage.Scene.renderData

data Game = Game
  { _mainViewport  :: Viewport Int
  , _gameScene     :: Scene HDRCamera GameEntity () ()
  , _sceneRenderer :: RenderSystem Game ()
  }

makeLenses ''Game

simScene :: YageWire t () (Scene HDRCamera GameEntity () ())
simScene = Scene
  <$> fmap singleton (acquireOnce testEntity)
  <*> pure ()
  <*> pure (defaultHDRCamera def)
  <*> pure ()

testEntity :: YageResource GameEntity
testEntity = Entity
  <$> (fromMesh =<< meshRes (loadYGM (\(YGMVertex p t tx tz) -> GVertex p t tx tz) ("res/sphere.ygm", mkSelection [])))
  <*> defaultGBaseMaterial
  <*> pure idTransformation


sceneWire :: YageWire t () Game
sceneWire = proc () -> do
  pipeline <- acquireOnce simplePipeline -< ()
  scene    <- simScene -< ()
  returnA -< Game (defaultViewport 800 600) scene pipeline



simplePipeline :: YageResource (RenderSystem Game ())
simplePipeline = do
  -- Convert output linear RGB to SRGB
  throwWithStack $ glEnable GL_FRAMEBUFFER_SRGB
  throwWithStack $
    io (getDir "res/glsl") >>= \ ss -> buildNamedStrings ss ("/res/glsl"</>)

  baseSampler <- mkBaseSampler
  gBasePass   <- drawGBuffers
  screenQuadPass <- drawRectangle


  return $ do
    game <- ask
    screenQuadPass .
      dimap (\game -> (game^.gameScene, game^.mainViewport))
            (\base -> ([(1,baseSampler,base^.aBuffer)], game^.mainViewport))
            gBasePass

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

  setVertexAttribute aPosition $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (undefined :: Vec3)) nullPtr)
  setVertexAttribute aColor    $= Just (Layout 3 GL_FLOAT False (2 * sizeOf (undefined :: Vec3)) (nullPtr `plusPtr` (sizeOf (undefined :: Vec3))))

  colorTex  <- mkSlot $ createTexture2D GL_TEXTURE_2D 1 1 :: YageResource (Slot (Texture PixelRGBA8))
  depthBuff <- createRenderbuffer 1 1 :: YageResource RenderbufferD24F
  fbo       <- glResource
  lastViewportRef     <- newIORef (defaultViewport 1 1)

  -- RenderPass
  return $ do
    mainViewport <- view viewport
    lastViewport <- get lastViewportRef

    -- resizing the framebuffer
    when (mainViewport /= lastViewport) $ do
      Yage.glViewport    $= mainViewport^.rectangle
      lastViewportRef    $= mainViewport
      let V2 w h = mainViewport^.rectangle.extend
      modifyM colorTex $ \x -> resizeTexture2D x w h
      c <- get colorTex
      void $ resizeRenderbuffer depthBuff w h
      void $ attachFramebuffer fbo [mkAttachment c] (Just $ mkAttachment depthBuff) Nothing

    boundFramebuffer RWFramebuffer $= fbo

    glClearColor 1 0 1 1
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT
    glEnable GL_DEPTH_TEST

    boundVertexArray $= vao
    boundBufferAt ElementArrayBuffer $= ebo

    currentProgram $= def
    boundProgramPipeline $= pipeline^.pipelineProgram

    checkPipelineError pipeline

    {-# SCC glDrawElements #-} throwWithStack $
      glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_BYTE nullPtr

    get colorTex

mkBaseSampler :: YageResource Sampler
mkBaseSampler = throwWithStack $ do
  sampler <- glResource
  samplerParameteri sampler GL_TEXTURE_WRAP_S $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_WRAP_T $= GL_CLAMP_TO_EDGE
  samplerParameteri sampler GL_TEXTURE_MIN_FILTER $= GL_LINEAR
  samplerParameteri sampler GL_TEXTURE_MAG_FILTER $= GL_LINEAR
  when gl_EXT_texture_filter_anisotropic $
    samplerParameterf sampler GL_TEXTURE_MAX_ANISOTROPY_EXT $= 16
  return sampler

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
