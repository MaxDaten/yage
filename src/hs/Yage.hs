{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Yage where

import             Yage.Prelude
---------------------------------------------------------------------------------------------------
import             Control.Wire
---------------------------------------------------------------------------------------------------
import             Data.List                    ((++))
---------------------------------------------------------------------------------------------------
import             Yage.Types
import             Yage.Core.Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------

class HasRenderView a where
    getRenderView :: a -> RenderUnit


yageMain :: (HasRenderView v) => String -> WindowConfig -> YageMainWire s v -> Session IO s -> IO ()
yageMain title winConf wire session = do
    _ <- bracket 
            (return $ initialization wire session $ RenderTarget (0, 0) (800, 600) 2 0.1 100 True) -- TODO remove it real target
            finalization
            (\initState -> execApplication title defaultAppConfig $ 
                                basicWindowLoop winConf initState $
                                yageLoop)
    return ()


initialization :: YageMainWire s v -> Session IO s -> RenderTarget -> YageLoopState s v
initialization wire session renderTarget =
    let rConf = RenderConfig
            { _rcConfClearColor    = GL.Color4 0.3 0.3 0.3 0
            , _rcConfDebugNormals  = False
            , _rcConfWireframe     = False
            }
    in YageLoopState mempty (RenderSettings rConf renderTarget) wire session

finalization :: YageLoopState s v -> IO ()
finalization _ = return ()


yageLoop :: (HasRenderView v) 
        => Window
        -> YageInput
        -> YageLoopState s v -> Application AnyException (YageLoopState s v)
yageLoop win ins@(inputState, winEvents) loopSt = do
    (dt, s')        <- io $ stepSession $ loopSt^.currentSession
    (rView, w')     <- io $ runYage ins $ stepWire (loopSt^.currentWire) dt (Right ())
    
    updatedSt <- either 
        (\e -> handleError e >> return loopSt)
        (renderTheViews loopSt)
        rView
    

    return $ updatedSt & currentWire     .~ w'
                       & currentSession  .~ s'
    where
        handleError :: (Throws InternalException l, Show e) => e -> Application l ()
        handleError e = criticalM $ "err:" ++ show e
        
        renderTheViews :: (Throws InternalException l, Throws SomeException l, HasRenderView v) 
                       => YageLoopState s v -> v -> Application l (YageLoopState s v)
        renderTheViews state rView = do
            let theView     = getRenderView rView
                settings    = state^.renderSettings
                res         = state^.renderRes
            (res', _rlog) <- runRenderSystem [theView] settings res
            return $ state & renderRes .~ res'

---------------------------------------------------------------------------------------------------

