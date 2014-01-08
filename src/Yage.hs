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
import             Control.Concurrent.STM          (TVar, STM, atomically, modifyTVar', readTVarIO, readTVar, newTVarIO)
import             Control.Monad                   (liftM)
---------------------------------------------------------------------------------------------------
import             Yage.Types                      as Types
import             Yage.Wire                       as Wire
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering

import             Yage.UI

import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------


data YageLoopState time view = YageLoopState
    { _renderResources  :: RenderResources
    , _currentWire      :: YageWire time () view
    , _currentSession   :: YageSession time
    , _renderSettings   :: TVar RenderSettings
    , _inputState       :: TVar InputState
    }

---------------------------------------------------------------------------------------------------
makeLenses ''YageLoopState

---------------------------------------------------------------------------------------------------

--data InternalEventController ectr = InternalEventController 
--    { innerECtr :: ectr }

-- maybe we will use generics and deriving later on
instance EventCtr (YageLoopState t v) where
    --windowPositionCallback = windowPositionCallback . _eventCtr
    windowSizeCallback YageLoopState{_renderSettings} = return $ \_winH w h ->
        atomically $ modifyTVar' _renderSettings $ reRenderTarget.targetSize .~ V2 w h
    --windowCloseCallback    = windowCloseCallback . _eventCtr
    --windowRefreshCallback  = windowRefreshCallback . _eventCtr
    --windowFocusCallback    = windowFocusCallback . _eventCtr
    --windowIconifyCallback  = windowIconifyCallback . _eventCtr
    --cursorEnterCallback    = cursorEnterCallback . _eventCtr
    keyCallback YageLoopState{_inputState} = return $ \_winH key code state modifier ->
        atomically $ modifyTVar' _inputState $ keyboard.keyEvents <>~ [KeyEvent key code state modifier]
    
    cursorPositionCallback YageLoopState{_inputState} = return $ \_winH x y ->
        atomically $ modifyTVar' _inputState $ mouse.mousePosition .~ V2 x y
    
    mouseButtonCallback YageLoopState{_inputState} = return $ \_winH button state modifier ->
        atomically $ modifyTVar' _inputState $ mouse.mouseButtonEvents <>~ [MouseButtonEvent button state modifier]
    --scrollCallback         = scrollCallback . _eventCtr


yageMain :: (HasRenderView view, Real time)=> String -> WindowConfig -> YageWire time () view -> YageSession time -> IO ()
yageMain title winConf wire session = 
    let renderTarget  = RenderTarget (V2 0 0) (V2 800 600) 2 0.1 100 True  -- TODO remove it real target
        rConf         = RenderConfig
                        { _rcConfClearColor    = GL.Color4 0.3 0.3 0.3 0
                        , _rcConfDebugNormals  = False
                        , _rcConfWireframe     = False
                        }
        renderSettings = RenderSettings rConf renderTarget
    in do
    _ <- bracket 
            (initialization wire session <$> newTVarIO renderSettings <*> newTVarIO mempty)
            finalization
            (\initState -> execApplication title defaultAppConfig $ 
                                basicWindowLoop winConf initState $
                                yageLoop)
    return ()


initialization :: YageWire time () view -> YageSession time -> TVar RenderSettings -> TVar InputState -> YageLoopState time view
initialization wire session tRenderSettings tInputState = YageLoopState mempty wire session tRenderSettings tInputState

finalization :: YageLoopState t v -> IO ()
finalization _ = return ()


yageLoop :: (HasRenderView view) 
        => Window
        -> YageLoopState time view -> Application AnyException (YageLoopState time view)
yageLoop win preRenderState = do
    inputSt      <- io $ atomically $ readModifyTVar (preRenderState^.inputState) clearEvents
    (ds, s')     <- io $ stepSession $ preRenderState^.currentSession
    (rView, w')  <- io $ stepWire (preRenderState^.currentWire) (ds inputSt) (Right ())
    
    postRenderState <- either 
        (\err -> handleError err >> return preRenderState)
        (renderTheViews preRenderState)
        rView
    

    return $ postRenderState & currentWire     .~ w'
                             & currentSession  .~ s'
    
    where
        handleError :: (Throws InternalException l, Show err) => err -> Application l ()
        handleError err = criticalM $ "err:" ++ show err

        readModifyTVar :: TVar a -> (a -> a) -> STM a
        readModifyTVar tvar f = do
            var <- readTVar tvar
            modifyTVar' tvar f
            return var

        
        renderTheViews :: (Throws InternalException l, Throws SomeException l, HasRenderView v) 
                       => YageLoopState t v -> v -> Application l (YageLoopState t v)
        renderTheViews state rView = do
            let theView     = getRenderView rView
                res         = state^.renderResources
            settings <- io $ readTVarIO $ state^.renderSettings
            (res', _rlog) <- runRenderSystem (mkRenderSystem theView) settings res 
            return $ state & renderResources .~ res'

---------------------------------------------------------------------------------------------------

