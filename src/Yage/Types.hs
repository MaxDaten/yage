{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Types where

import           Yage.Prelude
---------------------------------------------------------------------------------------------------
import           Control.Monad.Reader
import           Control.Wire          hiding (Event)
---------------------------------------------------------------------------------------------------
import           Yage.Core.Application (InputState, WindowEvents)
import           Yage.Rendering
---------------------------------------------------------------------------------------------------

type YageInput = (InputState, WindowEvents)

---------------------------------------------------------------------------------------------------


-- type Yage = ReaderT YageInput IO

type YageTimedInput t = Timed t YageInput
type YageSession t  = Session IO (YageInput -> YageTimedInput t)
type YageWire t = Wire (YageTimedInput t) ({--error--}) IO

{--
runYage :: YageInput -> Yage a -> IO a
runYage input m = runReaderT m input
--}

---------------------------------------------------------------------------------------------------

class HasRenderView a where
    getRenderView :: a -> RenderUnit


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


