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
{-# LANGUAGE TupleSections              #-}

module Yage.Rendering.RenderSystem
  ( RenderSystem(runRenderSystem)
  , mkDynamicRenderPass
  , mkStaticRenderPass
  , mkStatefulRenderPass
  , HasRenderSystem(..)
  ) where

import           Yage.Lens                          hiding (elements)
import           Yage.Prelude                       hiding (Element, pass, first)

import           Control.Arrow
import           Control.Category

-- | a monadic Mealy
newtype RenderSystem m i o = RenderSystem { runRenderSystem :: i -> m (o, RenderSystem m i o) }

makeClassyFor "HasRenderSystem" "renderSystem" [] ''RenderSystem


mkDynamicRenderPass :: (i -> m (o, RenderSystem m i o)) -> RenderSystem m i o
mkDynamicRenderPass = RenderSystem
{-# INLINE mkDynamicRenderPass #-}

mkStaticRenderPass :: Functor m => (i -> m o) -> RenderSystem m i o
mkStaticRenderPass f = r where r = RenderSystem $ fmap (,r) . f
{-# INLINE mkStaticRenderPass #-}

mkStatefulRenderPass :: Monad m => (s -> i -> m (o,s)) -> s -> RenderSystem m i o
mkStatefulRenderPass f = go where
  go s = RenderSystem $ \i -> do
    (o, t) <- f s i
    return (o, go t)
{-# INLINE mkStatefulRenderPass #-}

instance Monad m => Functor (RenderSystem m i) where
  fmap f (RenderSystem sys) = RenderSystem $ sys >=> \(o,sys') -> return (f o, fmap f sys')

instance Monad m => Applicative (RenderSystem m i) where
  pure b = r where r = RenderSystem $ return . const (b,r)
  RenderSystem sysf <*> RenderSystem sysa = RenderSystem $ \i -> do
    (f, mf) <- sysf i
    (a, ma) <- sysa i
    return (f a, mf <*> ma)

instance Monad m => Arrow (RenderSystem m) where
  arr f = RenderSystem $ \i -> return (f i, arr f)
  first sys = RenderSystem $ \(b,d) -> do
    (c, sys') <- runRenderSystem sys b
    return ((c,d), first sys')

instance Monad m => Category (RenderSystem m) where
  id = RenderSystem $ return . (,id)
  RenderSystem mbc . RenderSystem mab = RenderSystem $ \a -> do
    (b, mab') <- mab a
    (c, mbc') <- mbc b
    return (c, mbc' . mab')

instance Monad m => Profunctor (RenderSystem m) where
  rmap = fmap
  -- lmap f p = RenderPass $ RWST $ \a st -> runRWST (runSys p) (f a) st
  dimap f g (RenderSystem sys) = RenderSystem $ \i -> do
    (o,sys') <- sys (f i)
    return (g o, dimap f g sys')
