module Yage.Wire.Resources where

import Yage.Prelude hiding (any)
import Yage.Resources

import Control.Wire
import Yage.Wire.Types



-- | `YageResource` allocation wire
--
-- allocates a `YageResource` which is never freed (!)
-- (beside garbage collection of the haskell data structure)
constResourceAllocationW :: YageResource a -> YageWire t b a
constResourceAllocationW res = mkGenN $ \_ -> do
    (_key, a) <- allocateAcquire res
    return $ (Right a, mkConst $ Right a)

constMeshW :: YageResource (Mesh vert) -> YageWire t b (Mesh vert)
constMeshW = constResourceAllocationW

constTextureW :: YageResource Texture -> YageWire t b Texture
constTextureW = constResourceAllocationW
