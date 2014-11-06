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
-- resource is free.
allocationOnEvent :: YageWire t (Event (YageResource a)) a
allocationOnEvent = off
    where
    off = mkGenN $ event
        ( return (Left (), off) )
        ( allocateRes )
    on releaseKey loaded = mkGenN $ event
        ( return (Right loaded, on releaseKey loaded) )
        ( \res -> release releaseKey >> allocateRes res )

    allocateRes = fmap (\(k,a) -> (Right a, on k a)) . allocateAcquire

