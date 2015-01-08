{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE LambdaCase             #-}
-- | A Resource Slot eventually that is either empty or filled with a 'YageResource'.
-- If a 'MSlot' is filled with a 'YageResources' and a new 'YageResources' is slotted
-- into this slot, the old 'YageResources' is freed
module Yage.Resource.MSlot
  (
  -- * Resource Slot
    MSlot, mkEmptySlot,
  ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index)

import           Data.Acquire                     as Acquire
import           Control.Monad.Trans.Resource     as Acquire

import           Yage.Resource.YageResource
import           Quine.StateVar

newtype MSlot a = MSlot (IORef (Maybe (ReleaseKey, a)))
  deriving (Typeable,Generic)

mkEmptySlot :: YageResource (MSlot a)
mkEmptySlot = mkAcquire (MSlot <$> newIORef Nothing) freeSlot where
  freeSlot (MSlot var) = do
    mr <- atomicModifyIORef' var $ \v -> (Nothing,v)
    case mr of
      Nothing -> return ()
      Just (key,_) -> release key

instance MonadResource m => HasSetter (MSlot a) (YageResource a) m where
  (MSlot ref) $= yr = do
    res <- allocateAcquire yr
    mold <- io $ atomicModifyIORef' ref $! \val -> (Just res, val)
    case mold of
      Just (key,_) -> release key
      Nothing      -> return ()

instance MonadIO m => HasGetter (MSlot a) m (Maybe a) where
  get (MSlot ref) = fmap snd `liftM` (io $ readIORef ref)

instance MonadResource m => HasUpdate (MSlot a) a m where
  (MSlot ref) $~ f = modifyIORef ref $ over (mapped._2) f
  (MSlot ref) $~! f = modifyIORef' ref $ over (mapped._2) f
