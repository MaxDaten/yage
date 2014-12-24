{-# LANGUAGE ExistentialQuantification           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Yage.Rendering.Resources.GL.Framebuffer
  ( Attachment
  , mkAttachment
  , createFramebuffer
  , attachFramebuffer
  , acquireFramebuffer
  ) where

import           Yage.Prelude
import           Yage.Rendering.GL
import           Yage.Resources

import           Control.Exception
import           Control.Monad (zipWithM_)
import           Data.Foldable
import           Foreign.Marshal.Array

import           Quine.GL.Framebuffer
import           Quine.StateVar
import           Yage.Rendering.Resources.GL.Base

data Attachment = forall a. FramebufferAttachment a => Attachment a


-- | creates a 'Framebuffer' from a list of color attachments one optional depth and one optional
-- stencil attachment. The color attachments will be indexed from 'GL_COLOR_ATTACHMENT0' to
-- 'GL_COLOR_ATTACHMENT0' + n all color buffers are enabled for drawing and the first color attachment
-- for reading
createFramebuffer :: [Attachment] -> Maybe Attachment -> Maybe Attachment -> Acquire Framebuffer
createFramebuffer colors mDepth mStencil = throwWithStack $ do
  fb <- glResource :: Acquire Framebuffer
  attachFramebuffer fb colors mDepth mStencil

attachFramebuffer :: (MonadIO m, Applicative m) => Framebuffer -> [Attachment] -> Maybe Attachment -> Maybe Attachment -> m Framebuffer
attachFramebuffer fb colors mDepth mStencil = throwWithStack $ do
  boundFramebuffer RWFramebuffer $= fb
  zipWithM_ (\i (Attachment a) -> attach RWFramebuffer (GL_COLOR_ATTACHMENT0 + i) a) [0..] colors
  traverse_ (\(Attachment a)   -> attach RWFramebuffer GL_DEPTH_ATTACHMENT a) mDepth
  traverse_ (\(Attachment a)   -> attach RWFramebuffer GL_DEPTH_ATTACHMENT a) mStencil
  let cs =  (+) GL_COLOR_ATTACHMENT0 . fromIntegral <$> [0.. (length colors)-1]

  glDrawBuffer GL_NONE
  glReadBuffer GL_NONE
  io $ withArray cs $ \ptr -> do
    glDrawBuffers (fromIntegral $ length colors) ptr
  traverse_ glReadBuffer $ listToMaybe cs
  mErr <- checkFramebufferStatus RWFramebuffer
  case mErr of
    Just err  -> throw err
    _         -> return fb

acquireFramebuffer :: [Acquire Attachment] -> Maybe (Acquire Attachment) -> Maybe (Acquire Attachment) -> Acquire Framebuffer
acquireFramebuffer colorsA mDepthA mStencilA = throwWithStack $
  join $ liftM3 createFramebuffer (sequence colorsA) (sequence mDepthA) (sequence mStencilA)

-- | wraps an instance of 'FramebufferAttachment' into an 'Attachment' to allow a homomorphic
-- color attachment list
mkAttachment :: FramebufferAttachment a => a -> Attachment
mkAttachment = Attachment
