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
    Slot, mkSlot, slot, readSlotResource
  ) where

import           Yage.Lens
import           Yage.Prelude                     hiding (Index
                                                  , atomicModifyIORef', newIORef
                                                  , readIORef, modifyIORef
                                                  , modifyIORef')

import           Data.Acquire                     as Acquire
import           Control.Monad.Trans.Resource     as Acquire
import           Control.Monad.Base

import           Data.IORef
import           Yage.Geometry
import           Yage.Rendering.Mesh
import qualified Yage.Formats.Obj                 as OBJ
import qualified Yage.Formats.Ygm                 as YGM
import qualified Yage.Formats.Font                as Font
import           Yage.Font                        ( FontTexture )
import           Yage.Image
import           Yage.Resource.YageResource
import           Yage.Texture.CubeImageLayout     as Cubemap
import           Quine.MipmapChain                as MipmapChain
import           Quine.Cubemap                    as Cubemap
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

readSlotResource :: Slot a -> YageResource a
readSlotResource (Slot ref) = io (readIORef ref) >>= either id (return . snd)

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
