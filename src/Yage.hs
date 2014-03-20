{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage
    ( yageMain, YageSimulation(..)
    , module Application
    , module YagePrelude
    ) where

import             Yage.Prelude                    as YagePrelude
import             Yage.Lens                       as Lens hiding ( Index )
import             Yage.Text                       as TF
---------------------------------------------------------------------------------------------------
import             Control.Concurrent.STM          (TVar, STM, atomically, modifyTVar', readTVarIO, readTVar, newTVarIO)
import             Control.Monad.State             (gets)
---------------------------------------------------------------------------------------------------
import             Yage.Wire                       as Wire hiding ((<+>))
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import             Yage.Rendering.Viewport
import             Yage.Pipeline.Deferred

import             Yage.UI
import             Yage.Scene                      hiding (YageResources)

---------------------------------------------------------------------------------------------------


data YageResources = YageResources
    { _resourceRegistry   :: !(ResourceRegistry GeoVertex)
    , _renderResources    :: !GLResources
    , _resourceLoader     :: !(ResourceLoader GeoVertex)
    }


data YageTiming = YageTiming
    { _totalTime        :: !Double
    , _loopSession      :: !(YageSession NominalDiffTime)
    , _accumulator      :: !Double
    }

data YageSimulation time scene = YageSimulation
    { _simWire          :: !(YageWire time () scene)
    , _simSession       :: !(YageSession time)
    , _simPrevState     :: !(Either () scene)
    , _simDT            :: !time
    }

data YageLoopState time scene = YageLoopState
    { _loadedResources  :: !YageResources
    , _simulation       :: !(YageSimulation time scene)
    , _timing           :: !YageTiming
    , _inputState       :: TVar InputState
    , _renderStats      :: RStatistics
    }

---------------------------------------------------------------------------------------------------
makeLenses ''YageTiming
makeLenses ''YageSimulation
makeLenses ''YageLoopState
makeLenses ''YageResources

---------------------------------------------------------------------------------------------------
--data InternalEventController ectr = InternalEventController 
--    { innerECtr :: ectr }

-- maybe we will use generics and deriving later on
instance EventCtr (YageLoopState t v) where
    --windowPositionCallback = windowPositionCallback . _eventCtr
    --framebufferSizeCallback YageLoopState{_viewport} = return $ \_winH w h ->
    --    atomically $ modifyTVar' _viewport $ vpSize .~ V2 w h
    --windowCloseCallback    = windowCloseCallback . _eventCtr
    --windowRefreshCallback  = windowRefreshCallback . _eventCtr
    --windowFocusCallback    = windowFocusCallback . _eventCtr
    --windowIconifyCallback  = windowIconifyCallback . _eventCtr
    --cursorEnterCallback    = cursorEnterCallback . _eventCtr
    keyCallback YageLoopState{_inputState} = return $ \_winH key code state modifier ->
        atomically $ modifyTVar' _inputState $ \i -> 
            i & keyboard.keyEvents             <>~ [KeyEvent key code state modifier]
              & keyboard.keysDown.contains key .~  (state == KeyState'Pressed || state == KeyState'Repeating)
    
    cursorPositionCallback YageLoopState{_inputState} = return $ \_winH x y ->
        atomically $ modifyTVar' _inputState $ mouse.mousePosition .~ V2 x y
    
    mouseButtonCallback YageLoopState{_inputState} = return $ \_winH button state modifier ->
        atomically $ modifyTVar' _inputState $ mouse.mouseButtonEvents <>~ [MouseButtonEvent button state modifier]
    --scrollCallback         = scrollCallback . _eventCtr




yageMain :: (HasScene scene GeoVertex LitVertex, Real time) 
         => String -> WindowConfig -> YageWire time () scene -> time -> IO ()
yageMain title winConf sim dt = 
    -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
    let initResources     = YageResources initialRegistry initialGLRenderResources deferredResourceLoader
        initSim           = YageSimulation sim (countSession dt) (Left ()) dt
    in do
    _ <- bracket 
            ( initialization initResources initSim <$> (newTVarIO mempty) )
            finalization
            ( \initState -> execApplication title defaultAppConfig $
                                basicWindowLoop winConf initState $
                                yageLoop
            )
    return ()


-- http://gafferongames.com/game-physics/fix-your-timestep/
yageLoop :: (HasScene scene GeoVertex LitVertex, Real time) 
        => Window
        -> YageLoopState time scene -> Application AnyException (YageLoopState time scene)
yageLoop win previousState = do
    inputSt            <- io $! atomically $ readModifyTVar (previousState^.inputState) clearEvents
    (frameDT, newSession)   <- io $ stepSession $ previousState^.timing.loopSession
    let iaccum              = (realToFrac $ dtime (frameDT inputSt)) + previousState^.timing.accumulator
        prevSimState        = previousState^.simulation.simPrevState
        s                   = previousState^.simulation.simSession
        w                   = previousState^.simulation.simWire
        dt                  = realToFrac $ previousState^.simulation.simDT
    
    -- step our global timing to integrate our simulation
    ((newSimState, nextSimSession, nextSimWire, newAccum), wireTime) <- ioTime $ simulate iaccum dt s w prevSimState inputSt
    
    nextState <- processRendering newSimState
    
    setDevStuff (nextState^.renderStats) wireTime
    
    return $ nextState & simulation.simWire      .~ nextSimWire
                       & simulation.simSession   .~ nextSimSession
                       & simulation.simPrevState .~ newSimState
                       & timing.loopSession      .~ newSession
                       & timing.accumulator      .~ newAccum
    
    where
        --processRendering :: (Throws InternalException l, Show err, HasScene scene GeoVertex LitVertex) => Either err scene -> Application l (YageLoopState time scene, RStatistics)
        processRendering (Left err)    = (criticalM $ "err:" ++ show err) >> return previousState
        processRendering (Right scene) = do
            (renderScene, newFileRes) <- runYageResources 
                                          ( previousState^.loadedResources.resourceLoader )
                                          ( loadSceneResources $ getScene scene )
                                          ( previousState^.loadedResources.resourceRegistry )
            renderTheScene renderScene $ previousState & loadedResources.resourceRegistry .~ newFileRes

        simulate accum dt s' w' state input
            | accum < dt = return (state, s', w', accum)
            | otherwise   = do
                (st, s, w) <- stepSimulation s' w' input
                simulate (accum - dt) dt s w st input 


        stepSimulation s' w' input = do
            (ds, s)           <- io $ stepSession s'
            (newState, w)     <- io $ stepWire w' (ds input) (Right ())
            return $! newState `seq` (newState, s, w)

        renderTheScene renderScene state = do
            winSt           <- io $ readTVarIO (winState win)
            let screenVP    = Viewport 0 $ winSt^.fbSize
                pipeline    = yDeferredLighting screenVP renderScene
            (res', stats)   <- runRenderSystem pipeline $ state^.loadedResources.renderResources
            return $ state & loadedResources.renderResources .~ res'
                           & renderStats .~ stats

        setDevStuff stats wiretime = do
            title   <- gets appTitle
            gcTime  <- gets appGCTime
            setWindowTitle win $ TF.unpack $ 
                TF.format "{} [Wire: {}ms | RES: {}ms | R: {}ms | GC: {}ms | ∑: {}ms]"
                ( title
                , TF.fixed 4 $ 1000 * wiretime
                , TF.fixed 4 $ 1000 * stats^.resourcingTime
                , TF.fixed 4 $ 1000 * stats^.renderingTime
                , TF.fixed 4 $ 1000 * gcTime
                , TF.fixed 4 $ 1000 * sum [wiretime, stats^.resourcingTime, stats^.renderingTime, gcTime]
                )


initialization :: YageResources
               -> YageSimulation time scene
               -> TVar InputState 
               -> YageLoopState time scene
initialization resources sim tInputState = YageLoopState resources sim loopTimingInit tInputState mempty


finalization :: YageLoopState t v -> IO ()
finalization _ = return ()


loopTimingInit :: YageTiming
loopTimingInit = YageTiming 0 clockSession 0

readModifyTVar :: TVar a -> (a -> a) -> STM a
readModifyTVar tvar f = do
    var <- readTVar tvar
    modifyTVar' tvar f
    return var


