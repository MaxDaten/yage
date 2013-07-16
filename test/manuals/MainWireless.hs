{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Main where


import qualified Yage as Y
import Yage.Rendering
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Rendering.WorldState
import Yage.Resources
import Yage.Rendering.Primitives
import Linear (V3(..))


main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
    loop scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop scene env st = do
            _ <- Y.processInput (application env)
            (_, st) <- runYageRenderer (renderScene scene) st env
            loop scene env st

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill s@RenderScene{..} = 
            let shader = YageShader "src/glsl/base.vert" "src/glsl/frag.frag"
                def = RenderDefinition (cubeMesh, shader)
            in s{entities = [SomeRenderable $ RenderEntity (V3 0.3 0.0 0.0) def]}