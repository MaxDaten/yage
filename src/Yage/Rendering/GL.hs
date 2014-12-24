{-# LANGUAGE CPP #-}
module Yage.Rendering.GL
  ( module GL
  , throwWithStack
  ) where

import Yage.Prelude hiding (catch, throwIO)

import Graphics.GL.Core45 as GL
import Graphics.GL.Types  as GL
#ifdef GL_ERRCHECK
import Control.Exception
import qualified Quine.GL.Error as GLErr
import Data.Data
import GHC.Stack
#endif

#ifdef GL_ERRCHECK
data GLError = GLError [String] [GLErr.Error] deriving (Eq,Typeable,Data,Generic)
instance Exception GLError
instance Show GLError where
  show (GLError stack es) = showList es . showString "@\n" $ renderStack stack
#endif


-- | Wrapws an action and throws OpenGL errors occured so far when the library is
-- compiled with 'GL_ERRCHECK'
throwWithStack :: (MonadIO m) => m a -> m a
#ifdef GL_ERRCHECK
throwWithStack ma = do
  x <- ma
  stack <- liftIO $ currentCallStack
  io $ evaluate x >> GLErr.throwErrors `catch` (\(GLErr.Errors es) -> throw (GLError stack es))
  return x
#else
throwWithStack = id
#endif
{-# INLINE throwWithStack #-}
