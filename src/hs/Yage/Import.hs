module Yage.Import (
	-- module Import,
	io
    , timeIO
	) where

import Control.Monad.IO.Class

import Text.Printf
import Control.Exception
import System.CPUTime


io :: MonadIO m => IO a -> m a
io = liftIO


timeIO :: MonadIO m => m a -> m a
timeIO f = do
    start <- io $! getCPUTime
    v <- f
    end <- io $! getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    io $! printf "Computation time: %0.5f sec\n" (diff :: Double)
    return $! v
