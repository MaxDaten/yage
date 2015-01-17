{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE LambdaCase             #-}

-- | A resource Slot filled with a 'YageResource'. Slotted 'YageResource's are freed when a new
-- 'YageResource' replaces the old one.
module Yage.Resource.Slot
  (
  -- * Resource Slot
    Slot, mkSlot, slot, modifyM
  ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index
                                                  , atomicModifyIORef', newIORef
                                                  , readIORef, modifyIORef
                                                  , modifyIORef')

import           Data.Acquire                     as Acquire
import           Control.Monad.Trans.Resource     as Acquire

import           Data.IORef
import           Yage.Resource.YageResource
import           Quine.StateVar

newtype Slot a = Slot (IORef (Either (YageResource a) (ReleaseKey, a)))

mkSlot :: YageResource a -> YageResource (Slot a)
mkSlot aq = mkAcquire (fmap Slot . newIORef . Left $ aq) free where
  free (Slot ref) = get ref >>= \case
    Left _ -> return ()
    Right (key,_) -> release key

slot :: MonadResource m => Slot a -> YageResource a -> m ()
slot (Slot ref) aq = liftIO $ atomicModifyIORef' ref (\val -> (Left aq, val)) >>= \case
    Left _ -> return ()
    Right (key,_) -> release key


modifyM :: MonadResource m => Slot a -> (a -> YageResource a) -> m ()
modifyM s m = do
  x <- get s
  slot s (m x)

-- danger it dupes resources
-- readSlotResource :: Slot a -> YageResource a
-- readSlotResource (Slot ref) = io (readIORef ref) >>= either id (return . snd)

instance MonadIO m => HasSetter (Slot a) (YageResource a) m where
  (Slot ref) $= yr = liftIO $ atomicModifyIORef' ref (\val -> (Left yr, val)) >>= \case
    Left _ -> return ()
    Right (key,_) -> release key

instance MonadResource m => HasGetter (Slot a) m a where
  get (Slot ref) = do
    eval <- liftIO $ readIORef ref
    case eval of
      Left aq -> allocateAcquire aq >>= \val -> liftIO $ atomicModifyIORef' ref $ \_ -> (Right val,snd val)
      Right (_,res) -> return res

instance MonadResource m => HasUpdate (Slot a) a m where
  (Slot ref) $~ f = liftIO $ modifyIORef ref $ bimap (fmap f) (over _2 f)
  (Slot ref) $~! f = liftIO $ modifyIORef' ref $ bimap (fmap f) (over _2 f)
