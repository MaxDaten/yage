module Yage.Wire.Resources where

import           Yage.Prelude              hiding (any, on)
import           Yage.Resources


import           Control.Wire
import           Control.Wire.Unsafe.Event
import           Yage.Wire.Types



-- | `YageResource` allocation wire
--
-- allocates a `YageResource` which is never freed (!)
-- (beside garbage collection of the haskell data structure)
constResourceAllocationW :: YageResource a -> YageWire t b a
constResourceAllocationW res = mkGenN $ \_ -> do
    (_key, a) <- allocateAcquire res
    return $ (Right a, mkConst $ Right a)
{-# INLINE constResourceAllocationW #-}

constMeshW :: YageResource (Mesh vert) -> YageWire t b (Mesh vert)
constMeshW = constResourceAllocationW
{-# INLINE constMeshW #-}

constTextureW :: YageResource Texture -> YageWire t b Texture
constTextureW = constResourceAllocationW
{-# INLINE constTextureW #-}

constFontW :: YageResource FontTexture -> YageWire t b FontTexture
constFontW = constResourceAllocationW
{-# INLINE constFontW #-}


-- | loads resources each time the carrying 'Event' occurs. The previous loaded
-- resource is freed.
allocationOnEvent :: YageWire t (Event (YageResource a)) (Event a)
allocationOnEvent = unloaded
    where
    unloaded =
        mkGenN $ event (return (Right NoEvent, unloaded)) (\res -> do
            (k, a) <- allocateAcquire res
            return (Right $ Event a, loaded k)
            )

    loaded key =
        mkGenN $ event (return (Right NoEvent, loaded key)) (\res -> do
            release key
            (k, a) <- allocateAcquire res
            return (Right $ Event a, loaded k)
            )

