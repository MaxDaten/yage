{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor, ExistentialQuantification #-}
module Yage.Types where

import Prelude hiding (id, (.)) -- reimported by Control.Wire
---------------------------------------------------------------------------------------------------
import             Foreign
import             Foreign.Marshal.Array
import             Foreign.Marshal.Alloc
import             Foreign.Storable
import             Foreign.C.String
import             Control.Concurrent
import             Control.Monad
import             Control.Exception
import             Control.Monad.Reader
import             Control.Monad.State
import             Control.Wire                 hiding (Position, window)
import             Data.Typeable
---------------------------------------------------------------------------------------------------
import qualified   Data.Set                     as Set
import			   Yage.Resources
import 			   Yage.Rendering.Types
---------------------------------------------------------------------------------------------------


data YageState = YageState
    { inputs      :: Set.Set Input
    , renderEnv   :: RenderEnv
    , renderState :: RenderState
    , resources   :: [String{--YageResource--}]		-- ^ should use a res-manager later on
    --, resources  :: YageResources
    }

---------------------------------------------------------------------------------------------------


newtype Yage a = Yage (StateT YageState IO a)
    deriving (Functor, Monad, MonadIO, MonadState YageState, Typeable)

data Input = Input
type Inputs = Set.Set Input

type YageWire = WireM Yage

---------------------------------------------------------------------------------------------------

runYage :: YageState -> Yage a -> IO (a, YageState)
runYage st (Yage a) = runStateT a st

---------------------------------------------------------------------------------------------------

putRenderEnv :: RenderEnv -> Yage ()
putRenderEnv env = get >>= \yst -> put yst{ renderEnv = env }


getRenderEnv :: Yage (RenderEnv)
getRenderEnv = gets renderEnv


getRenderConfig :: Yage (RenderConfig)
getRenderConfig = envConfig `liftM` getRenderEnv

putRenderConfig :: RenderConfig -> Yage ()
putRenderConfig conf = getRenderEnv >>= \env -> putRenderEnv env{ envConfig = conf }


--getResources :: Yage (YageResources)
--getResources = gets resources

