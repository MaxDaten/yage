{-# LANGUAGE UnicodeSyntax, RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Yage where
 
import             Prelude                      hiding (id, (.)) -- reimported by Control.Wire
---------------------------------------------------------------------------------------------------
import             Foreign
import             Foreign.Marshal.Array
import             Foreign.Marshal.Alloc
import             Foreign.Storable
import             Foreign.C.String
import             Control.Concurrent
import             Control.Monad
import             Control.Exception
import             Control.Monad.Reader
import             Control.Monad.State
import             Control.Wire                 hiding (Position, window)
import             Data.Typeable
---------------------------------------------------------------------------------------------------
import             System.Mem
import qualified   Data.Set                     as Set
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Types
import             Yage.Core.Raw.FFI
import             Yage.Rendering
import             Yage.Rendering.Types
import             Yage.Rendering.WorldState
---------------------------------------------------------------------------------------------------




yageMain :: YageWire () WorldState -> Session IO -> IO ()
yageMain wire session = do
    _ <- bracket 
            (initialization) 
            (finalization)
            (\st -> yageLoop st wire session)
    return ()


initialization :: IO (YageState)
initialization = do
    app <- mkApplication []
    win <- mkGLWindow
    
    resizeWindow win 800 600
    showWindow win

    let rEnv = YageRenderEnv app win (YRenderConfig (Color4 0.3 0.3 0.3 0))
    return $ YageState Set.empty rEnv initialRenderState []


finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () WorldState -> Session IO -> IO ()
yageLoop ystate' wire session = do
    (dt, s') <- sessionUpdate session

    ins <- processInput (application $ renderEnv $ ystate')
    let yst' = ystate' { inputs = ins }

    ((mx, w'), yst) <- runYage yst' $ stepWire wire dt ()
    
    either 
        (\e -> handleError e >> return (renderState yst))
        (\s -> drawScene' s (renderState yst) (renderEnv yst))
        mx

    yageLoop yst w' s'
    where
        handleError :: Show e => e -> IO ()
        handleError e = print $ "err:" ++ show e
        drawScene' :: WorldState -> RenderState -> YageRenderEnv -> IO (RenderState)
        drawScene' _ st env = do
            -- postProcessScene :: Scene -> RenderScene
            let scene = RenderScene []
            (_, rSt) <- runYageRenderer (drawScene scene) st env
            return rSt

---------------------------------------------------------------------------------------------------

processInput :: YApplication -> IO (Inputs)
processInput app = do
    processEvents app
    return (Set.empty) -- dummy

