{-# LANGUAGE Arrows, RecordWildCards #-}

module Main where

import Prelude hiding (id, (.)) -- reimported by Control.Wire

import Yage
import Yage.Wire
import Control.Wire
import Control.Monad.State

import Graphics.Rendering.OpenGL.GL (Color4(..))

import Debug.Trace

main :: IO ()
main = yageMain mainWire clockSession

mainWire :: YageWire a Scene
mainWire = --frameStat .
        pure Scene 
        . clearColorW . arr (abs . sin) . integral_ (0, 0, 0, 0) . arr (*0.6) . pure (0.33, 0.28, 0.41, 0) 


frameStat :: YageWire a a
frameStat = proc i -> do
  frameStat <- countFrame &&& avgFps 100 -< ()
  returnA -< traceShow (frameStat) i

clearColorW :: YageWire (Double, Double, Double, Double) ()
clearColorW = mkFixM $ \_ (cR, cG, cB, cA) -> do
    rConf <- getRenderConfig
    putRenderConfig rConf{ clearColor = Color4 cR cG cB cA }

    return $ Right ()
