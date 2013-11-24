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
import             Yage.Rendering.RenderScene
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------

type WorldState = ()-- dummy



yageMain :: String -> YageWire () WorldState -> Session IO -> IO ()
yageMain title wire session = do
    _ <- bracket 
            (initialization $ RenderTarget (0, 0) (800, 600) 2 0.1 100 True) -- TODO remove it real target
            finalization
            (\st -> execApplication title defaultAppConfig $ yageLoop st wire session)
    return ()


initialization :: RenderTarget -> IO YageState
initialization renderTarget = do
    let rConf = RenderConfig
            { _rcConfClearColor    = GL.Color4 0.3 0.3 0.3 0 -- TODO to rendertarget
            , _rcConfDebugNormals  = False 
            , _rcConfWireframe     = False
            }
    return $ YageState Set.empty mempty (RenderSettings rConf renderTarget)

finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () WorldState -> Session IO -> Application AnyException ()
yageLoop ystate' wire session = do
    ins <- Set.fromList <$> collectEvents
    let yst' = ystate' & inputs .~ ins

    (dt, s')        <- io $ sessionUpdate session
    ((mx, w'), yst) <- io $ runYage yst' $ stepWire wire dt ()
    
    res' <- either 
        (\e -> handleError e >> return (yst^.renderRes))
        (\_s -> renderScene' (yst^.renderRes) (yst^.renderSettings))
        mx
    

    yageLoop (yst & renderRes .~ res') w' s'
    where
        handleError :: (Throws InternalException l, Show e) => e -> Application l ()
        handleError e = criticalM $ "err:" ++ show e
        
        renderScene' :: (Throws InternalException l, Throws SomeException l) => RenderResources -> RenderSettings -> Application l RenderResources
        renderScene' res settings = do
            let scene = undefined -- emptyRenderScene (Camera3D fpsCamera (deg2rad 60.0))-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TODO TOOOOODOOOOOO !!!!!
            (res', _rlog) <- runRenderSystem [RenderUnit scene] settings res
            return res'
---------------------------------------------------------------------------------------------------

