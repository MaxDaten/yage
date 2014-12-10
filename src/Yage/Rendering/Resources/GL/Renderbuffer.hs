{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.Resources.GL.Renderbuffer
  ( Renderbuffer
  , createRenderbuffer
  , resizeRenderbuffer
  , module TextureFormat
  ) where

import           Yage.Prelude
import           Yage.Rendering.GL

import           Quine.GL.Renderbuffer
import           Quine.Image
import           Quine.StateVar
import           Yage.Rendering.Resources.GL.Base
import           Yage.Rendering.Resources.GL.TextureFormat as TextureFormat


createRenderbuffer :: ImageFormat a => Int -> Int -> Acquire (Renderbuffer a)
createRenderbuffer width height = do
  rbuff <- glResource
  resizeRenderbuffer rbuff width height

resizeRenderbuffer :: forall a m. (ImageFormat a, MonadIO m) => Renderbuffer a -> Int -> Int -> m (Renderbuffer a)
resizeRenderbuffer rbuff width height = do
  boundRenderbuffer RenderbufferTarget $= rbuff
  glRenderbufferStorage GL_RENDERBUFFER (fromIntegral $ internalFormat (Proxy::Proxy a)) (fromIntegral width) (fromIntegral height)
  return rbuff
