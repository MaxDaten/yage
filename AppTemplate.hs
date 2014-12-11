{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Yage
import Yage.Wire
import Yage.Lens


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
  { _mainViewport :: Viewport Int
  }

makeLenses ''Scene

sceneWire :: YageWire t () Scene
sceneWire = pure $ Scene (undefined)

sceneRenderer = return ()


main :: IO ()
main = yageMain "standalone" appConf winSettings sceneWire sceneRenderer (1/60)

instance HasViewport Scene Int where
  viewport = mainViewport

instance LinearInterpolatable Scene where
  lerp _ _ = id
