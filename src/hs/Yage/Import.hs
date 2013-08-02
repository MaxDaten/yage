module Yage.Import (
	-- module Import,
	  io
    , printIOTime, ioTime
    , traceShow'
	) where

import Control.Monad.IO.Class

import Text.Printf
import Control.Exception
import System.CPUTime
import Debug.Trace


io :: MonadIO m => IO a -> m a
io = liftIO


ioTime :: MonadIO m => m a -> m (a, Double)
ioTime op = do
    start <- io $! getCPUTime
    v <- op
    end <- io $! getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    return $! (v, diff)


printIOTime :: MonadIO m => m a -> m a
printIOTime f = do
    (res, t) <- ioTime f
    io $! printf "Computation time: %0.5f sec\n" t
    return res

traceShow' :: Show a => a -> a
traceShow' a = traceShow a a