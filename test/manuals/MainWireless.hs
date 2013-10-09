{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards, OverloadedStrings #-}
module Main where

import Yage.Prelude

import Control.Monad (unless)
import qualified Yage as Y
import Yage.Core.Application
import Yage.Core.Application.Logging

import Yage.Rendering
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Resources
import Yage.Rendering.Primitives
import Linear (V3(..), axisAngle, point, signorm, axisAngle)
import Graphics.GLUtil.Camera3D (deg2rad)

hints = [ WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 2
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'OpenGLForwardCompat True]

main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
        conf = ApplicationConfig DEBUG
        
    print $ show $ length $ entities scene
    execApplication "MainWireless" conf $ do
        win <- createWindowWithHints hints 800 600 "MainWireless Window-0"
        makeContextCurrent $ Just win
        print <$> getWindowClientAPI win
        
        loop win scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop win scene env st = do
            _ <- Y.processEvents
            let scene' = updateScene scene

            swapBuffers win
            (_, st', _l) <- io $ runRenderer (renderScene scene') st env

            quit <- windowShouldClose win
            --print $ show $ renderStatistics st
            unless quit $ loop win scene' env st'
        
        updateScene :: RenderScene -> RenderScene
        updateScene scene = case (fromRenderable (head $ entities scene)) of
            (Just ent) -> 
                let rot = axisAngle (signorm $ V3 1 1 1) (deg2rad 0.5)
                in scene { entities = [SomeRenderable $ ent{ eOrientation = signorm $ (eOrientation ent) * rot}]
                         , sceneTime = 0.001 + sceneTime scene
                         }
            Nothing -> scene

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill s@RenderScene{..} = 
            let shader = YageShaderResource "src/glsl/simple.vert" "src/glsl/simple.frag"
                ent = (mkRenderEntity $ RenderDefinition (cubeMesh, shader))
                        { eScale = V3 2 2 2
                        }
                --singleBox = ent {ePosition = point $ V3 x y (-z)} | x <- [-5..5], y <- [-5..5], z <- [15..20]]
                singleBox = ent {ePosition = point $ V3 0 0 (-10)}
            in s{entities = [SomeRenderable singleBox]}