{-# LANGUAGE Arrows, RecordWildCards, OverloadedStrings #-}

module Main where

import Yage.Rendering.WorldState
import Control.Wire


import Yage
import Yage.Wire
import Yage.Types


main :: IO ()
main = yageMain "MainWire" mainWire clockSession

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