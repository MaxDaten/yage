{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}

module Yage.Core.API where

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Yage.Core.Types

--data Application i a = Application { runApp :: i a }
--data Window w = Window { getWin :: w }


class AppImpl i w m | i -> w m where
    --execApplication :: i a -> m b
    initApplication :: i Bool
    disposeApplication :: i ()
    createWindow :: Int -> Int -> String -> i w
    disposeWindow :: String -> i ()

{--
--instance (AppImpl i w m) => AppImpl (Application i) w m where
--    execApplication app = execApplication $ runApp app
--    initApplication = Application $ initApplication
--    disposeApplication = Application $ disposeApplication
--    createWindow w h t = Application $ createWindow w h t


--instance (Monad i) => Monad (Application i) where
--    return = Application . return
--    m >>= k = Application $ do
--        a <- runApp m
--        runApp (k a)
--    fail str = Application $ fail str


--instance MonadTrans (Application) where
--    lift = Application


--instance (MonadIO i) => MonadIO (Application i) where
--    liftIO = lift . liftIO

--}

