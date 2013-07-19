{-# LANGUAGE RankNTypes, StandaloneDeriving, RecordWildCards #-}
module Yage.Rendering.WorldState where

---------------------------------------------------------------------------------------------------
import              Graphics.GLUtil.Camera3D    (Camera, fpsCamera)

import qualified    Yage.Resources      as Res

---------------------------------------------------------------------------------------------------

data WorldState = WorldState 
    { camera    :: Camera Double
    , world     :: YageWorld            -- ^ the simulation world
    }
    --deriving (Show)

--deriving instance Show a => Show (Camera a)


-- | abstract data structure to hold a render-independent logical world
data YageWorld = YageWorld
    { worldEntities :: [YageWorldEntity]        -- ^ insert smart structure
    }

-- maybe use complete res-definition reference here?
-- pro: we can manipulate resources directly
-- cons: unwanted behavior, changes to this resource, the id doesn't correspond
-- discuss it later 
data YageWorldEntity = YageWorldEntity
    { resourceId :: Int }--Res.ResourceId }            -- ^ resource id to ref the definition

---------------------------------------------------------------------------------------------------

emptyWorldState :: WorldState
emptyWorldState = WorldState fpsCamera emptyWorld


emptyWorld :: YageWorld
emptyWorld = YageWorld []


addEntity :: YageWorldEntity -> YageWorld -> YageWorld
addEntity e w = w{ worldEntities = e:(worldEntities w) }


