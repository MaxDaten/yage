{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage
    ( yageMain, YageSimulation(..)
    , module Application
    , module YagePrelude
    , module Logging
    ) where

import             Yage.Prelude                    as YagePrelude
import             Yage.Lens                       as Lens hiding ( Index )
---------------------------------------------------------------------------------------------------
import             Data.Foldable                   (traverse_)
---------------------------------------------------------------------------------------------------
import             Control.Monad.State             (gets)
---------------------------------------------------------------------------------------------------
import             Yage.Wire                       as Wire hiding ((<+>))
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging   as Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import             Yage.Pipeline.Types
import             Yage.Pipeline.Deferred

import             Yage.UI
import             Yage.Scene                      hiding (YageResources)
import             Yage.Viewport                   (Viewport(..))
import             Yage.Transformation


---------------------------------------------------------------------------------------------------


data YageResources = YageResources
    { _resourceRegistry   :: !(ResourceRegistry GeoVertex)
    , _rhiResources       :: !GLResources
    , _resourceLoader     :: !(ResourceLoader GeoVertex)
    }


data YageTiming = YageTiming
    { _totalTime        :: !Double
    , _loopSession      :: !(YageSession NominalDiffTime)
    , _remainderAccum   :: !Double
    }

data YageSimulation time scene = YageSimulation
    { _simWire          :: !(YageWire time () scene)
    , _simSession       :: !(YageSession time)
    , _simScene         :: !(Either () scene)
    , _simDeltaT        :: !Double
    }

data YageLoopState time wScene rScene = YageLoopState
    { _loadedResources  :: !YageResources
    , _simulation       :: !(YageSimulation time wScene)
    , _pipeline         :: YageRenderSystem rScene
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
instance EventCtr (YageLoopState t a b) where
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


yageMain :: ( HasResources GeoVertex scene' scene, LinearInterpolatable scene', Real time ) => 
         String -> 
         ApplicationConfig ->
         WindowConfig -> 
         YageWire time () scene' -> 
         YageRenderSystem scene ->
         time -> IO ()
yageMain title appConf winConf sim thePipeline dt = 
    -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
    let initSim           = YageSimulation sim (countSession dt) (Left ()) $ realToFrac dt
    in do
    _ <- bracket 
            ( initialization initSim thePipeline <$> (newTVarIO mempty) )
            ( finalization )
            ( \initState -> execApplication title appConf $
                                basicWindowLoop winConf initState $
                                yageLoop
            )
    return ()


-- http://gafferongames.com/game-physics/fix-your-timestep/
yageLoop :: (Real time, HasResources GeoVertex wScene rScene, LinearInterpolatable wScene) => 
         Window ->
         YageLoopState time wScene rScene -> 
         Application AnyException (YageLoopState time wScene rScene)
yageLoop win oldState = do
    inputSt                 <- io $! atomically $ readModifyTVar (oldState^.inputState) clearEvents
    (frameDT, newSession)   <- io $ stepSession $ oldState^.timing.loopSession
    let currentRemainder    = (realToFrac $ dtime (frameDT inputSt)) + oldState^.timing.remainderAccum
    
    -- step our global timing to integrate our simulation
    ( ( renderSim, newSim, newRemainder ), simTime ) <- ioTime $ simulate currentRemainder ( oldState^.simulation ) inputSt
    
    newLoopState <- processRendering $ renderSim^.simScene
    
    setDevStuff (newLoopState^.renderStats) simTime
    

    -- log loaded resources and render trace
    traverse_ ( noticeM . show . format "[ResourceLog]: {}" . Only . Shown ) (newLoopState^.renderStats.resourceLog)
    traverse_ ( noticeM . show . format "[RenderTrace]: {}" . Only . Shown ) (newLoopState^.renderStats.renderLog.rlTrace)
    
    return $! newLoopState  
        & simulation              .~ newSim
        & timing.loopSession      .~ newSession
        & timing.remainderAccum   .~ newRemainder
    
    where
    
    -- | logic simulation
    -- selects small time increments and perform simulation steps to catch up withe the frame time
    simulate remainder sim input = simulate' remainder sim sim input
    
    simulate' remainder currentSim prevSim input
        | remainder < ( currentSim^.simDeltaT ) = return ( lerp (remainder / currentSim^.simDeltaT) prevSim currentSim, currentSim, remainder )
        | otherwise = do
            nextSim <- stepSimulation currentSim input
            simulate' ( remainder - currentSim^.simDeltaT ) nextSim currentSim input 

    -- perform one step in simulation
    stepSimulation sim input = do
        ( ds      , s )     <- io $ stepSession $ sim^.simSession
        ( newScene, w )     <- io $ stepWire (sim^.simWire) (ds input) (Right ())
        return $! newScene `seq` ( sim & simScene     .~ newScene 
                                       & simSession   .~ s 
                                       & simWire      .~ w )


    -- | loading resources & rendering
    processRendering (Left err)    = ( criticalM $ "err:" ++ show err ) >> return oldState
    processRendering (Right scene) = do
        (renderScene, newFileRes) <- runYageResources 
                                      ( oldState^.loadedResources.resourceLoader )
                                      ( requestResources scene )
                                      ( oldState^.loadedResources.resourceRegistry )
        renderTheScene renderScene $ oldState & loadedResources.resourceRegistry .~ newFileRes


    renderTheScene renderScene resourceState = do
        let thePipeline = oldState^.pipeline
        winSt           <- io $ readTVarIO ( winState win )
        (res', stats)   <- runRenderSystem ( thePipeline ( Viewport 0 (winSt^.fbSize) 2.2 ) renderScene ) 
                                           ( resourceState^.loadedResources.rhiResources )
        return $ resourceState & loadedResources.rhiResources .~ res'
                               & renderStats                  .~ stats


    -- debug & stats
    setDevStuff stats simTime = do
        title   <- gets appTitle
        gcTime  <- gets appGCTime
        setWindowTitle win $ unpack $ 
            format "{} [Wire: {}ms | RES: {}ms | R: {}ms | GC: {}ms | ∑: {}ms]"
            ( title
            , fixed 4 $ 1000 * simTime
            , fixed 4 $ 1000 * stats^.resourcingTime
            , fixed 4 $ 1000 * stats^.renderingTime
            , fixed 4 $ 1000 * gcTime
            , fixed 4 $ 1000 * sum [simTime, stats^.resourcingTime, stats^.renderingTime, gcTime]
            )


initialization :: YageSimulation time wScene
               -> YageRenderSystem rScene
               -> TVar InputState
               -> YageLoopState time wScene rScene
initialization sim thePipeline tInputState = 
    YageLoopState 
    { _loadedResources  = YageResources initialRegistry initialGLRenderResources deferredResourceLoader -- TODO deferred resource loaded to pipeline
    , _simulation       = sim
    , _pipeline         = thePipeline
    , _timing           = loopTimingInit
    , _inputState       = tInputState
    , _renderStats      = mempty
    }


finalization :: YageLoopState t a b -> IO ()
finalization _ = return ()


loopTimingInit :: YageTiming
loopTimingInit = YageTiming 0 clockSession 0

readModifyTVar :: TVar a -> (a -> a) -> STM a
readModifyTVar tvar f = do
    var <- readTVar tvar
    modifyTVar' tvar f
    return var

instance LinearInterpolatable scene => LinearInterpolatable (YageSimulation t scene) where
    lerp alpha u v = u & simScene .~ (lerp alpha <$> u^.simScene <*> v^.simScene)
