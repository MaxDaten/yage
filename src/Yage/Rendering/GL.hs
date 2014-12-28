{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.GL
  ( module GL
  , GLError(..)
  , throwWithStack
  , gl
  ) where

import Yage.Prelude hiding (throwIO)
import qualified Yage.Prelude as P
import Yage.Core.Application
import Graphics.GL.Core45 as GL
import Graphics.GL.Types  as GL
#ifdef GL_ERRCHECK
import qualified Control.Exception as E (evaluate, catch, throw)
import qualified Quine.GL.Error as GLErr
import Data.Data
import GHC.Stack
#endif

data GLError = GLError [String] [GLErr.Error] deriving (Eq,Typeable,Data,Generic)
instance Exception GLError
instance Show GLError where
  show (GLError stack es) = showList es . showString "@\n" $ renderStack stack

-- | Wrapws an action and throws OpenGL errors occured so far when the library is
-- compiled with 'GL_ERRCHECK'
throwWithStack :: (MonadIO m) => m a -> m a
#ifdef GL_ERRCHECK
throwWithStack ma = do
  x <- ma
  stack <- liftIO $ currentCallStack
  io $ E.evaluate x >> GLErr.throwErrors `E.catch` (\(GLErr.Errors es) -> E.throw (GLError stack es))
  return x
#else
throwWithStack = id
#endif
{-# INLINE throwWithStack #-}

gl :: (Throws GLError l) => Application l a -> Application l a
gl m = throwWithStack m
  `P.catch` (\(e::GLError) -> throw e)
