{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.Resources.GL.Renderbuffer
  ( Renderbuffer
  , createRenderbuffer
  , resizeRenderbuffer
  , module TextureFormat
  -- * Handy Aliases
  , RenderbufferD24F
  , RenderbufferD32F
  ) where

import           Yage.Prelude
import           Yage.Rendering.GL

import           Quine.GL.Renderbuffer
import           Quine.Image
import           Quine.StateVar
import           Yage.Rendering.Resources.GL.Base
import           Yage.Rendering.Resources.GL.TextureFormat as TextureFormat

type RenderbufferD24F = Renderbuffer (DepthComponent24 Float)
type RenderbufferD32F = Renderbuffer (DepthComponent32 Float)

createRenderbuffer :: ImageFormat a => Int -> Int -> Acquire (Renderbuffer a)
createRenderbuffer width height = do
  rbuff <- glResource
  resizeRenderbuffer rbuff width height

resizeRenderbuffer :: forall a m. (ImageFormat a, MonadIO m) => Renderbuffer a -> Int -> Int -> m (Renderbuffer a)
resizeRenderbuffer rbuff width height = do
  boundRenderbuffer RenderbufferTarget $= rbuff
  glRenderbufferStorage GL_RENDERBUFFER (fromIntegral $ internalFormat (Proxy::Proxy a)) (fromIntegral width) (fromIntegral height)
  return rbuff
