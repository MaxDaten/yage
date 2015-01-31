{-# OPTIONS_GHC -Wall            #-} -- REMOVE ME
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Yage.Rendering.RenderSystem
  ( RenderSystem(runRenderSystem)
  , Pass(Pass)
  , PassGEnv(PassGEnv)
  , PassEnv(..)
  , globalEnv, localEnv
  , processPass
  , processPassWithGlobalEnv
  , passEnvironment
  , mkDynamicRenderPass
  , mkStaticRenderPass
  , mkStatefulRenderPass
  , staticResource
  , HasRenderSystem(..)
  ) where

import           Yage.Lens                          hiding (elements)
import           Yage.Prelude                       hiding (Element, pass, first)

import           Control.Arrow
import           Control.Category
import           Yage.Resources
import           Data.Data

-- | a monadic Mealy
-- transducer pattern : http://tonyday567.github.io/blog/pipes-v-machines/
newtype RenderSystem m i o = RenderSystem { runRenderSystem :: i -> m (o, RenderSystem m i o) }

makeClassyFor "HasRenderSystem" "renderSystem" [] ''RenderSystem

-- | wrapps global and local environment for a pass
data PassEnv g l = PassEnv
  { _globalEnv :: g
  , _localEnv  :: l
  } deriving (Show,Read,Ord,Eq,Data,Typeable,Generic)

makeLenses ''PassEnv

-- | A wrapper for a 'RenderSystem' command with a local environment
data Pass :: * -> (* -> *) -> * -> * -> * where
  Pass     :: e -> RenderSystem (ReaderT e m) i o -> Pass e m i o

-- | A wrapper for a 'RenderSystem' command with a local and global environment
data PassGEnv :: * -> * -> (* -> *) -> * -> * -> * where
  PassGEnv :: l -> RenderSystem (ReaderT (PassEnv g l) m) i o -> PassGEnv g l m i o

-- | unwrapps the 'Pass', run the 'RenderSystem' with the local environemnt
processPass :: Monad m => Pass e m i o -> RenderSystem m i o
processPass (Pass env pass) = mkDynamicRenderPass $ \i -> do
    (o, sys) <- flip runReaderT env (runRenderSystem pass i)
    return $ o `seq` (o, processPass (Pass env sys))

-- | Captures a global environment and unwrapps the 'Pass' to be runned by the 'RenderSystem'
processPassWithGlobalEnv :: MonadReader g m => PassGEnv g l m i o -> RenderSystem m i o
processPassWithGlobalEnv (PassGEnv local pass) = mkDynamicRenderPass $ \i -> do
    global <- ask
    (o, sys) <- flip runReaderT (PassEnv global local) (runRenderSystem pass i)
    return $ o `seq` (o, processPassWithGlobalEnv (PassGEnv local sys))

passEnvironment :: Lens' (Pass e m i o) e
passEnvironment = lens getter setter where
  getter (Pass env _) = env
  setter (Pass _ pass) env = Pass env pass

mkDynamicRenderPass :: (i -> m (o, RenderSystem m i o)) -> RenderSystem m i o
mkDynamicRenderPass = RenderSystem
{-# INLINE mkDynamicRenderPass #-}

mkStaticRenderPass :: Monad m => (i -> m o) -> RenderSystem m i o
mkStaticRenderPass f = r where r = RenderSystem $ liftM (,r) . f
{-# INLINE mkStaticRenderPass #-}

mkStatefulRenderPass :: Monad m => (s -> i -> m (o,s)) -> s -> RenderSystem m i o
mkStatefulRenderPass f = go where
  go s = RenderSystem $ \i -> do
    (o, t) <- f s i
    return $ o `seq` (o, go t)
{-# INLINE mkStatefulRenderPass #-}

-- | Allocates once and returns the resource till termination
staticResource :: MonadResource m => YageResource a -> RenderSystem m () a
staticResource res = initRes where
  initRes = mkDynamicRenderPass $ \() -> do
    (_key, a) <- allocateAcquire res
    return (a, pure $ a)

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
