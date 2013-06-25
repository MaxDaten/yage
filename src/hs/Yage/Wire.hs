{-# LANGUAGE Arrows #-}

module Yage.Wire where

import Prelude hiding (id, (.))
import Control.Wire
import Control.Monad.IO.Class (MonadIO, liftIO)
import Text.Printf



countFrame :: (Monad m) => Wire e m a Int
countFrame = countFrom 0 <<< 1


timeString :: (Monad m) => Wire e m a String
timeString = fmap (printf "%8.2f") time


impure f = mkFixM $ \_ x -> Right <$> f x
showW :: (MonadIO m, Functor m, Show a) => Wire e m a a
showW = impure (\x -> liftIO (putStrLn (show x)) >> return x )