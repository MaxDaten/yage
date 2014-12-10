{-# LANGUAGE CPP #-}
module Yage.Rendering.GL
  ( module GL
  , throwWith
  ) where

import Yage.Prelude hiding (catch, throwIO)

import Graphics.GL.Core45 as GL
import Graphics.GL.Types  as GL
#ifdef GL_ERRCHECK
import Control.Exception
import qualified Quine.GL.Error as GLErr
import Data.Data
#endif

#ifdef GL_ERRCHECK
data GLError = GLError String GLErr.Error deriving (Show,Eq,Typeable,Data,Generic)
instance Exception GLError
#endif


-- | Wrapws an action an throws OpenGL errors occured so far when the library is
-- compiled with 'GL_ERRCHECK'
throwWith :: (MonadIO m) => String -> m a -> m a
#ifdef GL_ERRCHECK
throwWith msg ma = do
    x <- ma
    io $ evaluate x >> GLErr.throwErrors `catch` (throw . GLError msg)
    return x
#else
throwWith _ = id
#endif
{-# INLINE throwWith #-}
