{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Yage.Rendering.Scene where

---------------------------------------------------------------------------------------------------
import              Graphics.GLUtil.Camera3D    (Camera, fpsCamera)

import              Yage.World

---------------------------------------------------------------------------------------------------

data Scene = Scene 
    { camera    :: Camera Double
    , world     :: YageWorld            -- ^ the simulation world
    }
    --deriving (Show)

--deriving instance Show a => Show (Camera a)

---------------------------------------------------------------------------------------------------

emptyScene :: Scene
emptyScene = Scene fpsCamera emptyWorld


