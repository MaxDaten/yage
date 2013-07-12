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
import qualified   Graphics.Rendering.OpenGL.GL as GL
import             Graphics.Rendering.OpenGL.GL (($=))

import             Yage.Import
import             Yage.Types
import             Yage.Core.Raw.FFI
import             Yage.Rendering
import             Yage.Rendering.Scene
---------------------------------------------------------------------------------------------------




yageMain :: YageWire () Scene -> Session IO -> IO ()
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

    let rEnv = YageRenderEnv app win (YRenderConfig (GL.Color4 0.3 0.3 0.3 0)) emptyYRenderResources
    -- this kicks the first frame, qt init its opengl context now
    -- neccessary to enable gl calls
    -- TODO: find a better solution
    runYageRenderer (beforeRender >> afterRender) rEnv
    return $ YageState Set.empty rEnv []


finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () Scene -> Session IO -> IO ()
yageLoop state wire session = do
    (dt, s') <- sessionUpdate session

    ins <- processInput (application $ renderEnv $ state)
    let st = state { inputs = ins }

    ((mx, w'), st') <- runYage st $ stepWire wire dt ()

    either handleError (drawScene' $ renderEnv st') mx

    yageLoop st' w' s'
    where
        handleError e = print $ "err:" ++ show e
        drawScene' :: YageRenderEnv -> Scene -> IO ()
        drawScene' env scene = runYageRenderer (drawScene $ extractRenderScene scene) env

---------------------------------------------------------------------------------------------------

processInput :: YApplication -> IO (Inputs)
processInput app = do
    processEvents app
    return (Set.empty) -- dummy

