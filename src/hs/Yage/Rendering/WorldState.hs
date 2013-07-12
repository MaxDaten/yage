{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Yage.Rendering.WorldState where

---------------------------------------------------------------------------------------------------
import              Graphics.GLUtil.Camera3D    (Camera, fpsCamera)

import              Yage.World

---------------------------------------------------------------------------------------------------

data WorldState = WorldState 
    { camera    :: Camera Double
    , world     :: YageWorld            -- ^ the simulation world
    }
    --deriving (Show)

--deriving instance Show a => Show (Camera a)

---------------------------------------------------------------------------------------------------

emptyWorldState :: WorldState
emptyWorldState = WorldState fpsCamera emptyWorld


