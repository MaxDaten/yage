{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Main where


import qualified Yage as Y
import Yage.Rendering
import Yage.World
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Rendering.WorldState

main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
    loop scene (renderEnv state)

    Y.finalization state
    where 
        loop scene env = do
            _ <- Y.processInput (application env)
            runYageRenderer (drawScene scene) env
            loop scene env

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill s@RenderScene{..} = 
            --let entity = YageWorldEntity 0
            s{entities = []}