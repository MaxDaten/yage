{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage
    ( yageMain, YageSimulation(..)
    -- * Configuration
    , HasApplicationConfig(..)
    , HasWindowConfig(..)
    -- ** Reexports
    , MonitorOptions(..)
    , HasMonitorOptions(..)
    -- * Reexports
    , module Application
    , module Resource
    , module YagePrelude
    , module Logging
    , module Viewport
    , module Linear
    , module Transformation
    , module RenderSystem
    , module Rectangle
    , module Quine
    ) where

import             Yage.Prelude                    as YagePrelude
import             Yage.Lens                       as Lens hiding ( Index )
---------------------------------------------------------------------------------------------------
import             Data.Foldable                   (traverse_)
---------------------------------------------------------------------------------------------------
import             Control.Monad.State
import             Control.Exception
import             Control.Concurrent
import             Control.Monad.Trans.Resource    as Resource
---------------------------------------------------------------------------------------------------
import             Yage.Wire                       as Wire hiding ( (<+>), at, force )
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging   as Logging
import             Yage.Core.Application.Exception hiding (bracket)

import             Linear                          as Linear hiding (lerp, trace)
import             Yage.Geometry.D2.Rectangle      as Rectangle
import             Yage.Rendering.RenderSystem     as RenderSystem
import             Yage.Transformation             as Transformation
import             Yage.UI
import             Yage.Viewport                   as Viewport
import             Quine.StateVar                  as Quine

import             Quine.Monitor
---------------------------------------------------------------------------------------------------

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

data YageLoopState time scene = YageLoopState
    { _simulation         :: !(YageSimulation time scene)
    , _timing             :: !YageTiming
    , _inputState         :: TVar InputState
    , _metrics            :: Metrics
    }

data Metrics = Metrics
  { _monitor          :: !Monitor
  , _simulationFrames :: !Counter
  , _renderFrames     :: !Counter
  }

---------------------------------------------------------------------------------------------------
makeLenses ''YageTiming
makeLenses ''YageSimulation
makeClassy ''YageLoopState
makeLenses ''Metrics

makeClassy ''ApplicationConfig
makeClassy ''ApplicationState
makeClassy ''WindowConfig

---------------------------------------------------------------------------------------------------
--data InternalEventController ectr = InternalEventController
--    { innerECtr :: ectr }

-- maybe we will use generics and deriving later on
instance EventCtr (YageLoopState t s) where
    --windowPositionCallback = windowPositionCallback . _eventCtr
    --framebufferSizeCallback YageLoopState{_viewport} = return $ \_winH w h ->
    --    atomically $ modifyTVar' _viewport $ vpSize .~ V2 w h
    --windowCloseCallback    = windowCloseCallback . _eventCtr
    --windowRefreshCallback  = windowRefreshCallback . _eventCtr
    --windowFocusCallback    = windowFocusCallback . _eventCtr
    --windowIconifyCallback  = windowIconifyCallback . _eventCtr
    --cursorEnterCallback    = cursorEnterCallback . _eventCtr
    keyCallback YageLoopState{_inputState} = return $ \_winH key code state modifier -> do
        atomically $! modifyTVar' _inputState $!! \i ->
            i & keyboardEvents   <>~ [KeyEvent key code state modifier]

    cursorPositionCallback YageLoopState{_inputState} = return $ \_winH x y ->
        atomically $! modifyTVar' _inputState $!! mouseEvents <>~ [MouseMoveEvent (V2 x y)]

    mouseButtonCallback YageLoopState{_inputState} = return $ \_winH button state modifier ->
        atomically $! modifyTVar' _inputState $!! mouseEvents <>~ [MouseButtonEvent button state modifier]
    --scrollCallback         = scrollCallback . _eventCtr


yageMain ::
    ( Real time
    , LinearInterpolatable scene, HasViewport scene Int, HasRenderSystem scene IO scene ()
    , HasApplicationConfig conf
    , HasWindowConfig conf
    , HasMonitorOptions conf )
    => String -> conf -> YageWire time () scene -> time -> IO ()
yageMain title config sim dt =
  -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
  let initSim           = YageSimulation sim (countSession dt) (Left ()) $ realToFrac dt
      appConf           = config^.applicationConfig
      winConf           = config^.windowConfig
  in do
    tInputState <- newTVarIO mempty

    ekg <- forkMonitor (config^.monitorOptions)
    simCounter    <- counter "yage.simulation_frames" ekg
    renderCounter <- counter "yage.render_frames" ekg
    let metric = Metrics ekg simCounter renderCounter
        initState = YageLoopState
                    { _simulation         = initSim
                    , _timing             = loopTimingInit
                    , _inputState         = tInputState
                    , _metrics            = metric
                    }
    execApplication title appConf $ do
      win <- createWindowWithHints (windowHints winConf) (fst $ windowSize winConf) (snd $ windowSize winConf) title
      registerWindowCallbacks win initState
      makeContextCurrent $ Just win

      evalStateT (core win) initState

  where

  core win = forever $ do
    lift $ pollEvents
    input <- use inputState >>= (\var -> io $ atomically $ var `readModifyTVar` clearEvents)
    remAccum <- use (timing.remainderAccum)
    lift $ debugM $ format "{}" (Only $ Shown input)

    -- step out core session to get elasped time
    (frameDT, newSession)   <- io.stepSession =<< use (timing.loopSession)
    let currentRemainder    = realToFrac (dtime (frameDT input)) + remAccum

    -- process multiple simulation steps to catch up
    sim <- use simulation
    cnt <- use (metrics.simulationFrames)
    ((renderSim, newSim, newRemainder), simTime) <- ioTime $ liftResourceT $ simulate currentRemainder sim input cnt
    -- render simulation representation
    (_,renderTime) <-  ioTime $ case renderSim^.simScene of
        Left err    -> lift $ criticalM $ "err:" ++ show err
        Right scene -> renderTheScene scene win

    setDevStuff simTime renderTime win

    simulation            .= newSim

    timing.loopSession    .= newSession
    timing.remainderAccum .= newRemainder
    lift $ swapBuffers win

  -- | logic simulation
  -- selects small time increments and perform simulation steps to catch up with the frame time
  simulate remainder sim input cnt = {-# SCC simulate #-} do
      -- initial step with 0.0 deltaT and input (inject input into system)
      ( newScene, w )     <- stepWire (sim^.simWire) (Timed 0 input) (Right ())
      simulate' remainder (sim & simScene .~ newScene & simWire .~ w) sim cnt

  simulate' remainder currentSim prevSim cnt
      | remainder < ( currentSim^.simDeltaT ) = do
          return ( lerp (remainder / currentSim^.simDeltaT) prevSim currentSim, currentSim, remainder )
      | otherwise = do
          nextSim <- stepSimulation currentSim cnt
          simulate' ( remainder - currentSim^.simDeltaT ) nextSim currentSim cnt

  -- perform one step in simulation
  stepSimulation sim cnt = do
      inc cnt
      ( ds      , s )     <- io $ stepSession $ sim^.simSession
      ( newScene, w )     <- stepWire (sim^.simWire) (ds mempty) (Right ())
      return $! newScene `seq` ( sim & simScene     .~ newScene
                                     & simSession   .~ s
                                     & simWire      .~ w )


  renderTheScene scene win = do
    inc =<< use (metrics.renderFrames)
    winSt           <- io $ readTVarIO ( winState win )
    let fbRect      = Rectangle 0 $ winSt^.fbSize
        ratio       = (fromIntegral <$> winSt^.fbSize) / (fromIntegral <$> winSt^.winSize)
    {-# SCC rendering #-} io $ runPipeline (scene & viewport .~ Viewport fbRect ratio 2.2) (scene^.renderSystem)


  -- debug & stats
  setDevStuff simTime renderTime win = lift $ do
      title   <- gets appTitle
      gcTime  <- gets appGCTime
      setWindowTitle win $ unpack $
          format "{} [Sim: {}ms | R: {}ms | GC: {}ms | ∑: {}ms]"
                  ( title
                  , fixed 4 $ 1000 * simTime
                  , fixed 4 $ 1000 * renderTime
                  , fixed 4 $ 1000 * gcTime
                  , prec 4 $ 1000 * sum [simTime, gcTime] )

loopTimingInit :: YageTiming
loopTimingInit = YageTiming 0 clockSession 0

readModifyTVar :: TVar a -> (a -> a) -> STM a
readModifyTVar tvar f = do
    var <- readTVar tvar
    modifyTVar' tvar f
    return var
{-# INLINE readModifyTVar #-}


instance LinearInterpolatable scene => LinearInterpolatable (YageSimulation t scene) where
    lerp alpha u v = u & simScene .~ (lerp alpha <$> u^.simScene <*> v^.simScene)
