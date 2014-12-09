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
  ) where

import Yage.Prelude
import Yage.Core.OpenGL

import Control.Monad
import Control.Exception
import Data.Foldable

import Quine.StateVar
import Quine.GL.Framebuffer
import Yage.Rendering.Backend.Resource

data Attachment = forall a. FramebufferAttachment a => Attachment a

-- | creates a 'Framebuffer' from a list of color attachments one optional depth and one optional
-- stencil attachment. The color attachments will be indexed from 'GL_COLOR_ATTACHMENT0' to
-- 'GL_COLOR_ATTACHMENT0' + n
createFramebuffer :: [Attachment] -> Maybe Attachment -> Maybe Attachment -> Acquire Framebuffer
createFramebuffer colors mDepth mStencil = do
  fb <- glResource :: Acquire Framebuffer
  boundFramebuffer RWFramebuffer $= fb
  zipWithM_ (\i (Attachment a) -> attach RWFramebuffer (GL_COLOR_ATTACHMENT0 + i) a) [0..] colors
  traverse_ (\(Attachment a)   -> attach RWFramebuffer GL_DEPTH_ATTACHMENT a) mDepth
  traverse_ (\(Attachment a)   -> attach RWFramebuffer GL_DEPTH_ATTACHMENT a) mStencil

  mErr <- checkFramebufferStatus RWFramebuffer
  case mErr of
    Just err  -> throw err
    _         -> return fb

-- | wraps an instance of 'FramebufferAttachment' into an 'Attachment' to allow a homomorphic
-- color attachment list
mkAttachment :: FramebufferAttachment a => a -> Attachment
mkAttachment = Attachment
