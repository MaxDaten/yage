{-# LANGUAGE Arrows, RecordWildCards, OverloadedStrings #-}

module Main where


import Prelude hiding (id, (.)) -- reimported by Control.Wire
import Yage.Rendering.WorldState
import Control.Wire
import Control.Wire.Wire (never, constant)
import Control.Monad.State

import Graphics.GLUtil.Camera3D (fpsCamera)
import Graphics.GLUtil

import Yage
import Yage.Import
import Yage.Wire
import Yage.Types
import Yage.Rendering.Primitives


main :: IO ()
main = yageMain mainWire clockSession

mainWire :: YageWire a WorldState
mainWire = --frameStat . frameStat .
        scenWithCube <<<
        renderSetupW


frameStat :: YageWire a (Int, Double)
frameStat = countFrame &&& avgFps 100


scenWithCube :: YageWire a WorldState
scenWithCube = baseScene

    --combine <<< cube &&& baseScene
    --where
    --    combine :: YageWire (SomeRenderable, Scene) Scene
    --    combine =  arr snd


baseScene :: YageWire a WorldState
baseScene = pure emptyWorldState


--cube :: YageWire a SomeRenderable
--cube = yageObjectW cubeMesh


--yageObjectW :: TriMesh -> YageWire a SomeRenderable -- wrap it later on
--yageObjectW mesh = produceOnce (renderable mesh shader)
--    where shader = undefined


-- | Produces every step (!) the renderable 
--renderable :: TriMesh -> ShaderProgram -> YageWire a SomeRenderable
--renderable tri sh = perform . pure (liftM SomeRenderable . io $ mkRenderEntity tri sh)


renderSetupW :: YageWire a ()
renderSetupW = clearColorW . arr (fmap (abs . sin)) . colorW . arr (0.6*) . pure (0.33, 0.28, 0.41, 0)