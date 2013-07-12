module Yage.World where


import qualified 	Yage.Resources 		as Res
---------------------------------------------------------------------------------------------------

-- | abstract data structure to hold a render-independent logical world
data YageWorld = YageWorld
	{ worldEntities :: [YageWorldEntity] 		-- ^ insert smart structure
	}

-- maybe use complete res-definition reference here?
-- pro: we can manipulate resources directly
-- cons: unwanted behavior, changes to this resource, the id doesn't correspond
-- discuss it later 
data YageWorldEntity = YageWorldEntity
	{ resourceId :: Res.ResourceId }			-- ^ resource id to ref the definition

---------------------------------------------------------------------------------------------------

emptyWorld :: YageWorld
emptyWorld = YageWorld []

---------------------------------------------------------------------------------------------------

addEntity :: YageWorldEntity -> YageWorld -> YageWorld
addEntity e w = w{ worldEntities = e:(worldEntities w) }