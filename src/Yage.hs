{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage
    ( yageMain, YageSimulation(..), MovementKeys(..)
    , module Application
    , module YagePrelude
    , module Logging
    ) where

import             Yage.Prelude                    as YagePrelude
import             Yage.Lens                       as Lens hiding ( Index )
import qualified   Yage.Text                       as TF
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

---------------------------------------------------------------------------------------------------


data YageResources = YageResources
    { _resourceRegistry   :: !(ResourceRegistry GeoVertex)
    , _rhiResources       :: !GLResources
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


yageMain :: ( HasResources GeoVertex scene' scene, Real time ) => 
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
yageLoop :: (Real time, HasResources GeoVertex wScene rScene) => 
         Window ->
         YageLoopState time wScene rScene -> 
         Application AnyException (YageLoopState time wScene rScene)
yageLoop win oldState = do
    inputSt                 <- io $! atomically $ readModifyTVar (oldState^.inputState) clearEvents
    (frameDT, newSession)   <- io $ stepSession $ oldState^.timing.loopSession
    let iaccum              = (realToFrac $ dtime (frameDT inputSt)) + oldState^.timing.accumulator
    
    -- step our global timing to integrate our simulation
    ( ( newSim, newAccum ), simTime ) <- ioTime $ simulate iaccum ( oldState^.simulation ) inputSt
    
    newLoopState <- processRendering $ newSim^.simScene
    
    setDevStuff (newLoopState^.renderStats) simTime
    

    -- log loaded resources and render trace
    traverse_ (noticeM . format "[ResourceLog]: {0}" . singleton) (newLoopState^.renderStats.resourceLog)
    traverse_ (noticeM . format "[RenderTrace]: {0}" . singleton) (newLoopState^.renderStats.renderLog.rlTrace)
    
    return $! newLoopState  
        & simulation              .~ newSim
        & timing.loopSession      .~ newSession
        & timing.accumulator      .~ newAccum
    
    where
    -- | logic simulation
    -- selects small time increments and
    simulate accum sim input
        | accum < ( sim^.simDeltaT ) = return ( sim, accum )
        | otherwise = do
            sim' <- stepSimulation sim input
            simulate ( accum - sim^.simDeltaT ) sim' input 

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
        (res', stats)   <- runRenderSystem ( thePipeline ( Viewport 0 $ winSt^.fbSize ) renderScene ) 
                                           ( resourceState^.loadedResources.rhiResources )
        return $ resourceState & loadedResources.rhiResources .~ res'
                               & renderStats                  .~ stats


    -- debug & stats
    setDevStuff stats simTime = do
        title   <- gets appTitle
        gcTime  <- gets appGCTime
        setWindowTitle win $ TF.unpack $ 
            TF.format "{} [Wire: {}ms | RES: {}ms | R: {}ms | GC: {}ms | ∑: {}ms]"
            ( title
            , TF.fixed 4 $ 1000 * simTime
            , TF.fixed 4 $ 1000 * stats^.resourcingTime
            , TF.fixed 4 $ 1000 * stats^.renderingTime
            , TF.fixed 4 $ 1000 * gcTime
            , TF.fixed 4 $ 1000 * sum [simTime, stats^.resourcingTime, stats^.renderingTime, gcTime]
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


