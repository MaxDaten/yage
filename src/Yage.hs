{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
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
---------------------------------------------------------------------------------------------------
import             Yage.Types                      as Types
import             Yage.Wire                       as Wire hiding ((<+>))
import             Yage.Core.Application           as Application
import             Yage.Core.Application.Loops
import             Yage.Core.Application.Logging
import             Yage.Core.Application.Exception hiding (bracket)
import             Yage.Rendering hiding (P3)
import             Yage.Rendering.Viewport
import             Yage.Pipeline.Deferred

import             Yage.UI
import             Yage.Scene
import             Yage.Primitives
import             Yage.Geometry
import             Yage.Rendering.Transformation

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


yageMain :: (HasScene scene GeoVertex, Real time) 
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


yageLoop :: (HasScene scene GeoVertex) 
        => Window
        -> YageLoopState time scene -> Application AnyException (YageLoopState time scene)
yageLoop _win preRenderState = do
    inputSt            <- io $ atomically $ readModifyTVar (preRenderState^.inputState) clearEvents
    (ds, s')           <- io $ stepSession $ preRenderState^.currentSession
    (scene, w')        <- io $ stepWire (preRenderState^.currentWire) (ds inputSt) (Right ())
    
    postRenderState <- either 
        (\err -> handleError err >> return preRenderState)
        (renderTheScene preRenderState)
        scene
    

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

        
        renderTheScene :: (Throws InternalException l, Throws SomeException l
                          , HasScene scene GeoVertex )
                       => YageLoopState t scene -> scene -> Application l (YageLoopState t scene)
        renderTheScene state scene = do
            theViewport     <- io $ readTVarIO $ state^.viewport
            let theScene    = getScene scene
                pipelineDef = yDeferredLightingDescr theViewport theScene
                theSystem   = createDeferredRenderSystem pipelineDef (SceneView theScene theViewport)

            (res', _rlog)   <- runRenderSystem theSystem $ state^.renderResources
            -- io $ print $ rlog
            return $ state & renderResources .~ res'




instance HasScreen (SceneView GeoVertex) ScrVertex where
    getScreen (SceneView _ vp) = 
        let q              = (vertices . triangles $ addQuadTex $ quad 1) :: [Vertex ScrVertex]
            dim            = realToFrac <$> vp^.vpSize
            transformation = idTransformation & transPosition .~ 0
                                              & transScale    .~ V3 ( dim^._x ) ( dim^._y ) (1)
            definition     =  RenderDefinition
                                { _rdefData     = Right $ makeMesh "SCREEN" q
                                , _rdefTextures = []
                                , _rdefMode     = Triangles
                                } 
        in RenderEntity transformation definition


addQuadTex :: Primitive (Vertex P3) -> Primitive (Vertex P3T2)
addQuadTex (Quad (Face a b c d)) = Quad $ Face  (a <+> texture2 =: (V2 0 1))
                                                (b <+> texture2 =: (V2 0 0))
                                                (c <+> texture2 =: (V2 1 0))
                                                (d <+> texture2 =: (V2 1 1))
addQuadTex _ = error "not a quad"

---------------------------------------------------------------------------------------------------


--instance (RealFloat a, Typeable a) => Renderable (Viewport a) (P3 "pos" GLfloat) where
--    renderDefinition _      =
--        let quadVerts = (vertices . triangles $ quad 1)
--        in RenderDefinition
--            { _rdefData     = makeMesh "screen" quadVerts
--            --, _rdefProgram  = (shader, shdef)
--            , _rdefTextures = []
--            , _rdefMode     = Triangles
--            }
--    renderTransformation vp = 
--        let dim  = realToFrac <$> vp^.vpSize
--        in idTransformation & transPosition .~ 0
--                            & transScale    .~ V3 ( dim^._x ) ( dim^._y ) (1)
