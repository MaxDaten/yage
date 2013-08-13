{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Main where


import qualified Yage as Y

import Control.Monad (replicateM)
import Data.Maybe
import Yage.Rendering
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Rendering.WorldState
import Yage.Resources
import Yage.Rendering.Primitives
import Linear (V3(..), axisAngle, point, signorm, (^+^), (*^), axisAngle)
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
            let scene' = updateScene scene

            (_, st) <- runYageRenderer (renderScene scene') st env
            --print $ show $ renderStatistics st
            loop scene' env st
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
            let shader = YageShaderResource "src/glsl/base.vert" "src/glsl/base.frag"
                ent = (mkRenderEntity $ RenderDefinition (cubeMesh, shader))
                        { eScale = V3 2 2 2
                        }
                --singleBox = ent {ePosition = point $ V3 x y (-z)} | x <- [-5..5], y <- [-5..5], z <- [15..20]]
                singleBox = ent {ePosition = point $ V3 0 0 (-10)}
            in s{entities = [SomeRenderable singleBox]}