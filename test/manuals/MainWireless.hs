{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Main where


import qualified Yage as Y

import Control.Monad (replicateM)
import Yage.Rendering
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Rendering.WorldState
import Yage.Resources
import Yage.Rendering.Primitives
import Linear (V3(..), axisAngle, point)
import Graphics.GLUtil.Camera3D (deg2rad)


main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
    print $ show $ length $ entities scene
    loop scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop scene env st = do
            _ <- Y.processInput (application env)
            (_, st) <- runYageRenderer (renderScene scene) st env
            --print $ show $ renderStatistics st
            loop scene{sceneTime = (sceneTime scene) + 0.001} env st

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill s@RenderScene{..} = 
            let shader = YageShaderResource "src/glsl/base.vert" "src/glsl/base.frag"
                ent = (mkRenderEntity $ RenderDefinition (cubeMesh, shader))
                        { eScale = V3 0.1 0.1 0.1
                        }
                tileFloor = take 1 [ent {ePosition = point $ V3 x y (-z)} | x <- [-5..5], y <- [-5..5], z <- [15..20]]
            in s{entities = map SomeRenderable tileFloor}