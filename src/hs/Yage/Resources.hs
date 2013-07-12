module Yage.Resources where



-- | A 'YageResource' is loaded into a 'RenderEntity'
data YageResource = YageResource
	{ resourceId 		 :: ResourceId
	, resourceDefinition :: ResourceDefinition
	}

type ResourceId = Int
data ResourceDefinition = YageResourceDefinition [ResourceDefinition]
						| YageModelResource
						| YageShaderResource
						| YageTextureResource
