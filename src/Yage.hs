{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}

module Yage
    ( yageMain, YageSimulation(..)
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
import             Control.Monad.State             (gets)
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
    }

---------------------------------------------------------------------------------------------------
makeLenses ''YageTiming
makeLenses ''YageSimulation
makeLenses ''YageLoopState

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


yageMain :: ( Real time, LinearInterpolatable scene, HasViewport scene Int, HasRenderSystem scene IO scene () ) =>
         String ->
         ApplicationConfig ->
         WindowConfig ->
         YageWire time () scene ->
         time -> IO ()
yageMain title appConf winConf sim dt =
    -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
    let initSim           = YageSimulation sim (countSession dt) (Left ()) $ realToFrac dt
    in do
        tInputState <- newTVarIO mempty
        let initState = YageLoopState
                        { _simulation         = initSim
                        , _timing             = loopTimingInit
                        , _inputState         = tInputState
                        }

        _ <- execApplication title appConf $ basicWindowLoop winConf initState $ yageLoop
        return ()


-- http://gafferongames.com/game-physics/fix-your-timestep/
yageLoop :: (Real time, LinearInterpolatable scene, HasViewport scene Int, HasRenderSystem scene IO scene ()) =>
    Window ->
    YageLoopState time scene ->
    Application AnyException (YageLoopState time scene)
yageLoop win oldState = do
    inputSt                 <- io $ atomically $ readModifyTVar (oldState^.inputState) clearEvents
    debugM $ format "{}" (Only $ Shown inputSt)

    (frameDT, newSession)   <- io $ stepSession $ oldState^.timing.loopSession
    let currentRemainder    = (realToFrac $ dtime (frameDT inputSt)) + oldState^.timing.remainderAccum

    -- step our global timing to integrate our simulation
    ( ( renderSim, newSim, newRemainder ), simTime ) <- ioTime $ liftResourceT $ simulate currentRemainder ( oldState^.simulation ) inputSt

    (_,renderTime) <-  ioTime $ case renderSim^.simScene of
        Left err    -> ( criticalM $ "err:" ++ show err )
        Right scene -> io $ renderTheScene scene

    setDevStuff simTime renderTime

    return $! oldState
        & simulation              .~ newSim
        & timing.loopSession      .~ newSession
        & timing.remainderAccum   .~ newRemainder

    where

    -- | logic simulation
    -- selects small time increments and perform simulation steps to catch up with the frame time
    simulate remainder sim input = {-# SCC simulate #-} do
        -- initial step with 0.0 deltaT and input (inject input into system)
        ( newScene, w )     <- stepWire (sim^.simWire) (Timed 0 input) (Right ())
        simulate' remainder (sim & simScene .~ newScene & simWire .~ w) sim

    simulate' remainder currentSim prevSim
        | remainder < ( currentSim^.simDeltaT ) = do
            return ( lerp (remainder / currentSim^.simDeltaT) prevSim currentSim, currentSim, remainder )
        | otherwise = do
            nextSim <- stepSimulation currentSim
            simulate' ( remainder - currentSim^.simDeltaT ) nextSim currentSim

    -- perform one step in simulation
    stepSimulation sim = do
        ( ds      , s )     <- io $ stepSession $ sim^.simSession
        ( newScene, w )     <- stepWire (sim^.simWire) (ds mempty) (Right ())
        return $! newScene `seq` ( sim & simScene     .~ newScene
                                       & simSession   .~ s
                                       & simWire      .~ w )


    renderTheScene scene = do
        winSt           <- io $ readTVarIO ( winState win )
        let fbRect      = Rectangle 0 $ winSt^.fbSize
            ratio       = (fromIntegral <$> winSt^.fbSize) / (fromIntegral <$> winSt^.winSize)
        {-# SCC rendering #-} runPipeline (scene & viewport .~ Viewport fbRect ratio 2.2) (scene^.renderSystem)


    -- debug & stats
    setDevStuff simTime renderTime = do
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
