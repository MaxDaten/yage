{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Yage
import Yage.Wire
import Yage.Lens
import Yage.GL


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

data Scene = Scene
  { _mainViewport  :: Viewport Int
  , _sceneRenderer :: RenderSystem IO Scene ()
  }

makeLenses ''Scene

sceneWire :: YageWire t () Scene
sceneWire = proc () -> do
  pipeline <- acquireOnce clearViewport -< ()
  returnA -< Scene (defaultViewport 800 600) pipeline

clearViewport = return $ do
  vp <- view viewport
  Yage.glViewport $= vp^.rectangle
  glClearColor 1 0 0 1
  glClear $ GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT

main :: IO ()
main = yageMain "standalone" appConf winSettings sceneWire (1/60)

instance HasViewport Scene Int where
  viewport = mainViewport

instance LinearInterpolatable Scene where
  lerp _ _ = id

instance HasRenderSystem Scene IO Scene () where
  renderSystem = sceneRenderer
