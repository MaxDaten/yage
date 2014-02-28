{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Yage
    ( yageMain, YageSimulation(..)
    , module Types
    , module Application
    , module YagePrelude
    ) where

import             Yage.Prelude                    as YagePrelude
import             Yage.Lens                       as Lens hiding ( Index )
---------------------------------------------------------------------------------------------------
import             Control.Concurrent.STM          (TVar, STM, atomically, modifyTVar', readTVarIO, readTVar, newTVarIO)
---------------------------------------------------------------------------------------------------
import             Yage.Types                      as Types
import             Yage.Wire                       as Wire hiding ((<+>))
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import             Yage.Rendering.Viewport
import             Yage.Pipeline.Deferred

import             Yage.UI
import             Yage.Scene
import qualified   Yage.Resources as Res

import Data.Time.Clock
---------------------------------------------------------------------------------------------------


data YageResources = YageResources
    { _resourceRegistry   :: !(Res.ResourceRegistry GeoVertex)
    , _renderResources    :: !GLResources
    , _resourceLoader     :: !(Res.ResourceLoader GeoVertex)
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
    , _viewport         :: TVar ViewportI
    , _inputState       :: TVar InputState
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
    windowSizeCallback YageLoopState{_viewport} = return $ \_winH w h ->
        atomically $ modifyTVar' _viewport $ vpSize .~ V2 w h
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


yageMain :: (HasScene scene GeoVertex, Real time) 
         => String -> WindowConfig -> YageWire time () scene -> time -> IO ()
yageMain title winConf sim dt = 
    -- http://www.glfw.org/docs/latest/news.html#news_30_hidpi
    let theViewport   = Viewport (V2 0 0) (uncurry V2 (windowSize winConf)) 2.0
        resources     = YageResources Res.initialRegistry initialGLRenderResources deferredResourceLoader
    in do
    _ <- bracket 
            ( initialization resources (YageSimulation sim (countSession dt) (Left ()) dt)
                <$> newTVarIO theViewport 
                <*> newTVarIO mempty
            )
            finalization
            ( \initState -> execApplication title defaultAppConfig $
                                basicWindowLoop winConf initState $
                                yageLoop
            )
    return ()


-- http://gafferongames.com/game-physics/fix-your-timestep/
yageLoop :: (HasScene scene GeoVertex, Real time) 
        => Window
        -> YageLoopState time scene -> Application AnyException (YageLoopState time scene)
yageLoop _win previousState = do
    inputSt            <- io $! atomically $ readModifyTVar (previousState^.inputState) clearEvents
    
    (frameDT, newSession)   <- io $ stepSession $ previousState^.timing.loopSession
    let iaccum              = (realToFrac $ dtime (frameDT inputSt)) + previousState^.timing.accumulator
        prevSimState        = previousState^.simulation.simPrevState
        s                   = previousState^.simulation.simSession
        w                   = previousState^.simulation.simWire
        dt                  = realToFrac $ previousState^.simulation.simDT
    
    -- step our global timing to integrate our simulation
    (simSt, simS, simW, newAccum) <- simulate iaccum dt s w prevSimState inputSt
    
    processRendering simSt  <&> simulation.simWire      .~ simW
                            <&> simulation.simSession   .~ simS
                            <&> simulation.simPrevState .~ simSt
                            <&> timing.loopSession      .~ newSession
                            <&> timing.accumulator      .~ newAccum
    
    where
        -- processRendering :: (Throws InternalException l, Show err, HasScene scene GeoVertex) => Either err scene -> Application l (YageLoopState time scene)
        processRendering (Left err)    = (criticalM $ "err:" ++ show err) >> return previousState
        processRendering (Right scene) = do
            (renderScene, newFileRes) <- Res.runYageResources 
                                          ( previousState^.loadedResources.resourceLoader )
                                          ( loadSceneResources $ getScene scene )
                                          ( previousState^.loadedResources.resourceRegistry )
            flip renderTheScene renderScene $ (previousState & loadedResources.resourceRegistry .~ newFileRes)

        simulate accum dt s' w' state input
            | accum < dt = return (state, s', w', accum)
            | otherwise   = do
                (st, s, w) <- stepSimulation s' w' input
                simulate (accum - dt) dt s w st input 


        stepSimulation s' w' input = do
            (ds, s)           <- io $ stepSession s'
            (newState, w)     <- io $ stepWire w' (ds input) (Right ())
            return $! newState `seq` (newState, s, w)

        renderTheScene :: (Throws InternalException l, Throws SomeException l)
                       => YageLoopState t scene -> SScene GeoVertex -> Application l (YageLoopState t scene)
        renderTheScene state renderScene = do
            theViewport     <- io $ readTVarIO $ state^.viewport
            let pipeline    = yDeferredLighting theViewport renderScene
            (res', _rlog)   <- runRenderSystem pipeline $ state^.loadedResources.renderResources
            -- io $ print $ rlog
            return $ state & loadedResources.renderResources .~ res'



initialization :: YageResources
               -> YageSimulation time scene
               -> TVar ViewportI
               -> TVar InputState 
               -> YageLoopState time scene
initialization resources sim tInputState = YageLoopState resources sim loopTimingInit tInputState

finalization :: YageLoopState t v -> IO ()
finalization _ = return ()


loopTimingInit :: YageTiming
loopTimingInit = YageTiming 0 clockSession 0

readModifyTVar :: TVar a -> (a -> a) -> STM a
readModifyTVar tvar f = do
    var <- readTVar tvar
    modifyTVar' tvar f
    return var


