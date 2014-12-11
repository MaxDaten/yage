{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Yage
import Yage.Wire
import Yage.Lens
import Yage.GL
import Quine.Monitor
import Control.Concurrent


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

data Scene = Scene
  { _mainViewport  :: Viewport Int
  , _sceneRenderer :: RenderSystem IO Scene ()
  }

makeLenses ''Scene

sceneWire :: YageWire t () Scene
sceneWire = proc () -> do
  pipeline <- acquireOnce clearViewport -< ()
  returnA -< Scene (defaultViewport 800 600) pipeline

clearViewport = do
  -- Convert output linear RGB to SRGB
  glEnable GL_FRAMEBUFFER_SRGB
  glClearColor 1 0 0 1
  return $ do
    vp <- view viewport
    Yage.glViewport $= vp^.rectangle
    glClear $ GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

main :: IO ()
main = yageMain "standalone" configuration sceneWire (1/60)

instance HasMonitorOptions Configuration where
  monitorOptions = mainMonitorOptions

instance HasWindowConfig Configuration where
  windowConfig = mainWindowConfig

instance HasApplicationConfig Configuration where
  applicationConfig = mainAppConfig

instance HasViewport Scene Int where
  viewport = mainViewport

instance LinearInterpolatable Scene where
  lerp _ _ = id

instance HasRenderSystem Scene IO Scene () where
  renderSystem = sceneRenderer
