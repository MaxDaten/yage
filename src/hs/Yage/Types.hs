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


type Yage = ReaderT YageInput IO

type NominalTimed = Timed NominalDiffTime ()
type YageMainWire session view = Wire session () Yage () view

data YageLoopState s v = YageLoopState
    { _renderRes        :: RenderResources
    , _renderSettings   :: RenderSettings
    , _currentWire      :: YageMainWire s v
    , _currentSession   :: Session IO s
    }

---------------------------------------------------------------------------------------------------
makeLenses ''YageLoopState

---------------------------------------------------------------------------------------------------

runYage :: YageInput -> Yage a -> IO a
runYage input m = runReaderT m input

---------------------------------------------------------------------------------------------------

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


