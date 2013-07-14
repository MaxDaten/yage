module Yage.Resources where

import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)


-- | A 'YageResource' is loaded into a 'RenderEntity'
data YageResource = YageResource
    { resourceId         :: ResourceId
    , resourceDefinition :: ResourceDefinition
    }

type ResourceId = Int
data ResourceDefinition = YageResourceDefinition [ResourceDefinition]
                        | YageModelResource TriMesh
                        | YageShaderResource YageShader
                        | YageTextureResource
                        deriving (Show, Eq)

allShaders :: YageResource -> [ResourceDefinition]
allShaders (YageResource _ s@(YageShaderResource _))       = [s]
allShaders (YageResource _ (YageResourceDefinition defs))  = filter isShader defs
allShaders _ = []

allTextures :: YageResource -> [ResourceDefinition]
allTextures (YageResource _ YageTextureResource)           = [YageTextureResource]
allTextures (YageResource _ (YageResourceDefinition defs)) = filter (==YageTextureResource) defs
allTextures _ = []

allModels :: YageResource -> [ResourceDefinition]
allModels (YageResource _ m@(YageModelResource _))         = [m]
allModels (YageResource _ (YageResourceDefinition defs))   = filter isModel defs
allModels _ = []


isModel :: ResourceDefinition -> Bool
isModel (YageModelResource _) = True
isModel _ = False

isShader :: ResourceDefinition -> Bool
isShader (YageShaderResource _) = True
isShader _ = False

isTexture :: ResourceDefinition -> Bool
isTexture (YageTextureResource) = True
isTexture _ = False

---------------------------------------------------------------------------------------------------

type Position = V3 Double
type Orientation = Quaternion Double

type Vertex = V3 Float
type Index = Int

data TriMesh = TriMesh
    { vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show, Eq)

mkTriMesh :: [Vertex] -> [Index] -> TriMesh
-- some assertions for invalid meshes
mkTriMesh vs ixs = TriMesh vs ixs ((length ixs) `quot` 3)

---------------------------------------------------------------------------------------------------

data YageShader = YageShader
    { shaderType :: YageShaderType
    , src        :: String
    } deriving (Show, Eq)

data YageShaderType = YageVertexShader
                    | YageFragmentShader
                    deriving (Show, Eq)

--type YageShaderSource = String

positionAttrib = "position"