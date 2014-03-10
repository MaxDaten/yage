{-# LANGUAGE FlexibleContexts #-}
module Yage.Types where

import           Yage.Prelude
---------------------------------------------------------------------------------------------------
import           Control.Wire          (Wire, Timed, Session)
---------------------------------------------------------------------------------------------------
import           Yage.UI
--import           Yage.Rendering.Vertex
---------------------------------------------------------------------------------------------------




type YageTimedInputState t = Timed t InputState
type YageSession t = Session IO (InputState -> YageTimedInputState t)
type YageWire t = Wire (YageTimedInputState t) ({--error--}) IO

{--
runYage :: YageInput -> Yage a -> IO a
runYage input m = runReaderT m input
--}




---------------------------------------------------------------------------------------------------

-- class (ViableVertex (Vertex geo)) => HasScene a geo where


{--
putRenderEnv :: RenderEnv -> Yage ()
putRenderEnv env = get >>= \yst -> put yst{ renderUnit = (renderUnit yst){ _renderSettings = env } }


getRenderEnv :: Yage RenderEnv
getRenderEnv = gets $ _renderSettings . renderUnit


getRenderConfig :: Yage RenderConfig
getRenderConfig = envConfig `liftM` getRenderEnv

putRenderConfig :: RenderConfig -> Yage ()
putRenderConfig conf = getRenderEnv >>= \env -> putRenderEnv env{ envConfig = conf }

--}

--getResources :: Yage (YageResources)
--getResources = gets resources


