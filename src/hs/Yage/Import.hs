module Yage.Import (
	module Import,
	io
	) where

import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL.GL as Import (($=))


io :: MonadIO m => IO a -> m a
io = liftIO