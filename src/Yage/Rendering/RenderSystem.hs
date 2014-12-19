{-# OPTIONS_GHC -Wall            #-} -- REMOVE ME
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Yage.Rendering.RenderSystem
  ( RenderSystemT
  , RenderSystem
  , HasRenderSystem(..)
  , runPipeline
  ) where

import           Yage.Lens                          hiding (elements)
import           Yage.Prelude                       hiding (Element, pass)

import           Control.Arrow
import           Control.Category
import           Control.Monad.RWS                  (RWST(..), runRWST)
import           Control.Monad.Base
import           Control.Monad.Trans.Resource

-- |
newtype RenderSystemT m a b = RenderPass { runSys :: RWST a () () m b }
  deriving (Functor, Applicative, Monad, MonadReader a)

type RenderSystem a b = (RenderSystemT (ResourceT IO) a b)

makeClassyFor "HasRenderSystem" "renderSystem" [] ''RenderSystemT

instance Monad m => Arrow (RenderSystemT m) where
  arr f = RenderPass $ do
    a <- ask
    return $ f a
  first s = RenderPass $ RWST $ \(b,d) st -> do
    (c, st', w) <- runRWST (runSys s) b st
    return ((c, d), st', w)

instance Monad m => Category (RenderSystemT m) where
  id = RenderPass $ RWST $ \a st -> return (a, st, mempty)
  f . g = RenderPass $ RWST $ \a st -> do
    (b, st', w) <- runRWST (runSys g) a st
    (c, st'', w') <- runRWST (runSys f) b st'
    return (c, st'', w `mappend` w')

instance Monad m => Profunctor (RenderSystemT m) where
  -- lmap f p = RenderPass $ RWST $ \a st -> runRWST (runSys p) (f a) st
  dimap f g p = RenderPass $ RWST $ \a st -> do
    (b, st', w) <- runRWST (runSys p) (f a) st
    return (g b, st', w)


runPipeline :: MonadIO m => scene -> RenderSystemT m scene t -> m t
runPipeline scene sys = do
  (t,_,_) <- runRWST (runSys sys) scene ()
  return t

instance MonadIO m => MonadIO (RenderSystemT m r) where
  liftIO = RenderPass . liftIO
instance MonadBase IO m => MonadBase IO (RenderSystemT m r) where
  liftBase = RenderPass . liftBase
instance MonadThrow m => MonadThrow (RenderSystemT m r) where
  throwM = RenderPass . throwM
instance MonadResource m => MonadResource (RenderSystemT m r) where
  liftResourceT = RenderPass . liftResourceT
