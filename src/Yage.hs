{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Yage
    ( yageMain
    , module Types
    , module Application
    , module YagePrelude
    ) where

import             Yage.Prelude                    as YagePrelude
---------------------------------------------------------------------------------------------------
import             Data.List                       ((++))
---------------------------------------------------------------------------------------------------
import             Yage.Types                      as Types
import             Yage.Wire                       as Wire
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------


data YageLoopState t v = YageLoopState
    { _renderRes        :: RenderResources
    , _renderSettings   :: RenderSettings
    , _currentWire      :: YageWire t () v
    , _currentSession   :: YageSession t
    }

---------------------------------------------------------------------------------------------------
makeLenses ''YageLoopState

---------------------------------------------------------------------------------------------------



yageMain :: (HasRenderView view, Real t) => String -> WindowConfig -> YageWire t () view -> YageSession t -> IO ()
yageMain title winConf wire session = do
    _ <- bracket 
            (return $ initialization wire session $ RenderTarget (0, 0) (800, 600) 2 0.1 100 True) -- TODO remove it real target
            finalization
            (\initState -> execApplication title defaultAppConfig $ 
                                basicWindowLoop winConf initState $
                                yageLoop)
    return ()


initialization :: YageWire t () view -> YageSession t -> RenderTarget -> YageLoopState t view
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
yageLoop _win events loopSt = do
    (ds, s')     <- io $ stepSession $ loopSt^.currentSession
    (rView, w') <- io $ stepWire (loopSt^.currentWire) (ds events) (Right ())
    
    updatedSt <- either 
        (\e -> handleError e >> return loopSt)
        (renderTheViews $ loopSt & renderSettings %~ internalSettingsUpdate events)
        rView
    

    return $ updatedSt & currentWire     .~ w'
                       & currentSession  .~ s'
    where
        handleError :: (Throws InternalException l, Show e) => e -> Application l ()
        handleError e = criticalM $ "err:" ++ show e

        internalSettingsUpdate :: YageInput -> RenderSettings -> RenderSettings
        internalSettingsUpdate (_, winEvents) settings = settings & reRenderTarget.targetSize %?~ justResizedTo winEvents
        
        renderTheViews :: (Throws InternalException l, Throws SomeException l, HasRenderView v) 
                       => YageLoopState s v -> v -> Application l (YageLoopState s v)
        renderTheViews state rView = do
            let theView     = getRenderView rView
                settings    = state^.renderSettings
                res         = state^.renderRes
            (res', _rlog) <- runRenderSystem [theView] settings res
            return $ state & renderRes .~ res'

---------------------------------------------------------------------------------------------------

