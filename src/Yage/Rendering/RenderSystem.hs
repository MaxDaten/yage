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
  ( RenderSystem
  , HasRenderSystem(..)
  , runPipeline
  ) where


import           Yage.Lens                          hiding (elements)
import           Yage.Prelude                       hiding (Element, pass)

import           Control.Arrow
import           Control.Category
import           Control.Monad.RWS                  (RWST(..), runRWST)

-- |
newtype RenderSystemT m a b = RenderPass { runSys :: RWST a () () m b }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader a)

type RenderSystem = RenderSystemT IO

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


{--
type Viewport = V2 Int
data Env = Env
  { _envViewport :: Viewport }

makeFields ''Env

newtype Element = Element ()
  deriving (Show)

data Scene = Scene
  { sceneElements :: [Element]
  , sceneViewport :: Viewport
  }
makeFields ''Scene


mkGeoPass :: (MonadIO m, Functor m, HasViewport env Viewport) => Acquire (RenderSystem m env (Texture PixelRGBA8, Texture PixelRGBA8, Texture PixelRGBA8))
mkGeoPass = do
  gBuffer@[tex1,tex2,tex3] <- replicateM 3 $ createTexture2D GL_TEXTURE_2D 256 256
  fb <- createFramebuffer (fmap mkAttachment gBuffer) Nothing Nothing
  return $ do
    boundFramebuffer RWFramebuffer $= fb
    vp@(V2 w h) <- view viewport
    forM_ gBuffer (\tex -> void $ resizeTexture2D tex w h)
    liftIO $ print $ "; viewport: " ++ show vp
    return (tex1, tex2, tex3)


lightPass :: MonadIO m => RenderSystem m (Texture PixelRGBA8, Texture PixelRGBA8,Texture PixelRGBA8) (Texture PixelRGBA8)
lightPass = do
  (a, b, c) <- ask
  liftIO $ print (a, b, c)
  return $ c

screenPass = undefined

main :: IO ()
main = -- the pipeline
  runResourceT $ do
    let vp = V2 800 600 :: Viewport
        scene    = Scene [] vp

    (_key, geoPass) <- allocateAcquire mkGeoPass
    runPipeline scene $ proc scene -> do
      -- lit <- lightPass . geoPass   -< scene
      gbuffer <- geoPass   -< scene
      lit     <- lightPass -< gbuffer
      screenPass  -< lit
  -- end pipeline
--}
