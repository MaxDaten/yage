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
import             Yage.Pipeline.Deferred

import             Yage.UI

import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------


data YageLoopState time view = YageLoopState
    { _renderResources  :: GLResources
    , _currentWire      :: YageWire time () view
    , _currentSession   :: YageSession time
    , _renderConfig     :: TVar RenderConfig
    , _viewport         :: TVar ViewportI
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
    windowSizeCallback YageLoopState{_viewport} = return $ \_winH w h ->
        atomically $ modifyTVar' _viewport $ vpSize .~ V2 w h
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


yageMain :: (HasRenderScene scene, Real time) 
         => String -> WindowConfig -> YageWire time () scene -> YageSession time -> IO ()
yageMain title winConf wire session = 
    -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
    let theViewport   = Viewport (V2 0 0) (uncurry V2 (windowSize winConf)) 2.0
        renderConf    = RenderConfig
                        { _rcConfDebugNormals  = False
                        , _rcConfWireframe     = False
                        }
    in do
    _ <- bracket 
            ( initialization wire session initialGLRenderResources
                <$> newTVarIO renderConf 
                <*> newTVarIO theViewport 
                <*> newTVarIO mempty
            )
            finalization
            ( \initState -> execApplication title defaultAppConfig $ 
                                basicWindowLoop winConf initState $
                                yageLoop
            )
    return ()


initialization :: 
                  YageWire time () scene 
               -> YageSession time
               -> GLResources
               -> TVar RenderConfig 
               -> TVar ViewportI
               -> TVar InputState 
               -> YageLoopState time scene
initialization wire session resources tRenderSettings tInputState = YageLoopState resources wire session tRenderSettings tInputState

finalization :: YageLoopState t v -> IO ()
finalization _ = return ()


yageLoop :: (HasRenderScene scene) 
        => Window
        -> YageLoopState time scene -> Application AnyException (YageLoopState time scene)
yageLoop win preRenderState = do
    inputSt            <- io $ atomically $ readModifyTVar (preRenderState^.inputState) clearEvents
    (ds, s')           <- io $ stepSession $ preRenderState^.currentSession
    (renderScene, w')  <- io $ stepWire (preRenderState^.currentWire) (ds inputSt) (Right ())
    
    postRenderState <- either 
        (\err -> handleError err >> return preRenderState)
        (renderTheScene preRenderState)
        renderScene
    

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

        
        renderTheScene :: (Throws InternalException l, Throws SomeException l, HasRenderScene v) 
                       => YageLoopState t v -> v -> Application l (YageLoopState t v)
        renderTheScene state v = do
            theViewport     <- io $ readTVarIO $ state^.viewport
            let theScene    = getRenderScene v
                pipelineDef = yDeferredLightingDescr theViewport
                theSystem   = createDeferredRenderSystem pipelineDef theScene (fromIntegral <$> theViewport)

            (res', _rlog)   <- runRenderSystem theSystem $ state^.renderResources 
            return $ state & renderResources .~ res'

---------------------------------------------------------------------------------------------------

