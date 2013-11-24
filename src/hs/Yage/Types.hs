{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Yage.Types where

import           Yage.Prelude
-- import Prelude hiding (id, (.)) -- reimported by Control.Wire
---------------------------------------------------------------------------------------------------
--import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Wire          hiding (Event, Position, window)
---------------------------------------------------------------------------------------------------
import qualified Data.Set              as Set
import           Yage.Core.Application (Event)
import           Yage.Rendering
---------------------------------------------------------------------------------------------------


data YageState = YageState
    { _inputs           :: Set.Set Event
    , _renderRes        :: RenderResources
    , _renderSettings   :: RenderSettings
    }

makeLenses ''YageState
---------------------------------------------------------------------------------------------------


newtype Yage a = Yage (StateT YageState IO a)
    deriving (Functor, Monad, MonadIO, MonadState YageState, Typeable)

--data Input = Input
--type Inputs = Set.Set Input

type YageWire = WireM Yage

---------------------------------------------------------------------------------------------------

runYage :: YageState -> Yage a -> IO (a, YageState)
runYage st (Yage a) = runStateT a st

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


