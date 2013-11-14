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
---------------------------------------------------------------------------------------------------

type WorldState = ()-- dummy



yageMain :: String -> YageWire () WorldState -> Session IO -> IO ()
yageMain title wire session = do
    _ <- bracket 
            initialization
            finalization
            (\st -> execApplication title defaultAppConfig $ yageLoop st wire session)
    return ()


initialization :: IO YageState
initialization = do
    let rConf = RenderConfig
            { confClearColor    = Color4 0.3 0.3 0.3 0 -- TODO to rendertarget
            , confDebugNormals  = False 
            , confWireframe     = False
            }
        renderTarget = RenderTarget (800, 600) 2 -- TODO real target
        rEnv         = RenderEnv rConf renderTarget
    return $ YageState Set.empty rEnv []


finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () WorldState -> Session IO -> Application AnyException ()
yageLoop ystate' wire session = do
    ins <- Set.fromList <$> collectEvents
    let yst' = ystate' { inputs = ins }

    (dt, s') <- io $ sessionUpdate session
    ((mx, w'), yst) <- io $ runYage yst' $ stepWire wire dt ()
    
    either 
        (\e -> handleError e)
        (\s -> renderScene' (renderEnv yst))
        mx

    yageLoop yst w' s'
    where
        handleError :: (Throws InternalException l, Show e) => e -> Application l ()
        handleError e = criticalM $ "err:" ++ show e
        
        renderScene' :: (Throws InternalException l) => RenderEnv -> Application l ()
        renderScene' env = do
            -- postProcessScene :: Scene -> RenderScene
            --let scene = emptyRenderScene -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TOOOOODOOOOOO !!!!!
            (_, rSt, rLog) <- ioe $ runRenderer (undefined) env
            debugM $ show rLog
            return $! rSt

---------------------------------------------------------------------------------------------------

