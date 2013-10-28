{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Yage where

import             Yage.Prelude 
---------------------------------------------------------------------------------------------------
import             Control.Wire                 hiding (Event, Position, window)
---------------------------------------------------------------------------------------------------
import qualified   Data.Set                     as Set
import             Data.List                    ((++))
---------------------------------------------------------------------------------------------------
import             Yage.Types
import             Yage.Core.Application
import             Yage.Core.Application.Utils
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering
import             Yage.Rendering.Types
import             Yage.Rendering.WorldState
import             Control.Lens
---------------------------------------------------------------------------------------------------




yageMain :: String -> YageWire () WorldState -> Session IO -> IO ()
yageMain title wire session = do
    let conf = ApplicationConfig DEBUG
    _ <- bracket 
            initialization
            finalization
            (\st -> execApplication title conf $ yageLoop st wire session)
    return ()


initialization :: IO YageState
initialization = do
    let rConf = RenderConfig
            { confClearColor = Color4 0.3 0.3 0.3 0
            , confDebugNormals = False 
            }
        renderTarget = RenderTarget (800, 600) 2 -- TODO real target
        rEnv = RenderEnv rConf renderTarget
    return $ YageState Set.empty rEnv initialRenderState []


finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () WorldState -> Session IO -> Application AnyException ()
yageLoop ystate' wire session = do
    ins <- processEvents
    let yst' = ystate' { inputs = ins }

    (dt, s') <- io $ sessionUpdate session
    ((mx, w'), yst) <- io $ runYage yst' $ stepWire wire dt ()
    
    either 
        (\e -> handleError e >> return (renderState yst))
        (\s -> renderScene' s (renderState yst) (renderEnv yst))
        mx

    yageLoop yst w' s'
    where
        handleError :: (Throws InternalException l, Show e) => e -> Application l ()
        handleError e = criticalM $ "err:" ++ show e
        
        renderScene' :: (Throws InternalException l) => WorldState -> RenderState -> RenderEnv -> Application l RenderState
        renderScene' _ st env = do
            -- postProcessScene :: Scene -> RenderScene
            let scene = emptyRenderScene -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TOOOOODOOOOOO !!!!!
            (_, rSt, rLog) <- ioe $ runRenderer (renderScene scene) st env
            debugM $ show rLog
            return $! rSt

---------------------------------------------------------------------------------------------------

processEvents :: (Throws InternalException l) => Application l (Set.Set Event)
processEvents = pollEvent >>= processEvent' Set.empty
    where 
        processEvent' ::  (Throws InternalException l) => Set.Set Event -> Maybe Event -> Application l (Set.Set Event)
        processEvent' es Nothing = return es
        processEvent' es (Just e) = do
            me <- pollEvent
            internalProcessEvent me
            processEvent' (Set.insert e es) me


keyWasPressed :: Key -> Set.Set Event -> Bool
keyWasPressed key keySet = not . Set.null $ keyPressed `Set.filter` keySet
    where 
        keyPressed (EventKey _winHandle ekey) 
            | ekey^._key == key = True
            | otherwise = False
        keyPressed _ = False



internalProcessEvent :: (Throws InternalException l) => Maybe Event -> Application l ()
internalProcessEvent Nothing = return ()
internalProcessEvent (Just e) = debugM $ show e

