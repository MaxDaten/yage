{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Main where


import qualified Yage as Y
import Yage.Rendering
import Yage.Types (YageState(..))
import Yage.Rendering.Types
import Yage.Rendering.WorldState

main :: IO ()
main = do
    state <- Y.initialization

    let scene = testScene
    loop scene (renderEnv state) (renderState state)

    Y.finalization state
    where 
        loop scene env st = do
            _ <- Y.processInput (application env)
            (_, st) <- runYageRenderer (drawScene scene) st env
            loop scene env st

testScene :: RenderScene
testScene = fill (emptyRenderScene)
    where
        fill s@RenderScene{..} = 
            --let entity = YageWorldEntity 0
            s{entities = []}