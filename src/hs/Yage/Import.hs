module Yage.Import (
	-- module Import,
	io
	) where

import Control.Monad.IO.Class


io :: MonadIO m => IO a -> m a
io = liftIO
