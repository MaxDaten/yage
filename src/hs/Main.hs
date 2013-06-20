{-# LANGUAGE Arrows #-}

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
mainWire = frameStat . pure Scene . clearColorW . time


frameStat :: YageWire a a
frameStat = proc i -> do
  frameStat <- countFrame &&& avgFps 100 -< ()
  returnA -< traceShow (frameStat) i

{--
clearColor :: YageWire Time b
clearColor = proc t -> do
	let c = abs(sin $ fromIntegral 3 / 100.0)
    io $ clearColor $= Color4 1 c c 0

anim
--}

clearColorW :: YageWire Time ()
clearColorW = mkFixM $ \_ t -> do
	let c = realToFrac $ abs(sin t)
	yst <- get
	put $ yst { clearColor = Color4 c c c 0 }
	--io $ clearColor $= Color4 c c c 0
	return $ Right ()