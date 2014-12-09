{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Rendering.Resources.GL.Renderbuffer
  ( Renderbuffer
  , createRenderbuffer
  , module Yage.Rendering.Backend.TextureFormat
  ) where

import           Yage.Core.OpenGL
import           Yage.Prelude

import           Data.Data
import qualified Quine.GL.Renderbuffer                as GL
import           Quine.Image
import           Quine.StateVar
import           Yage.Rendering.Backend.Resource
import           Yage.Rendering.Backend.TextureFormat



newtype Renderbuffer a = Renderbuffer (GL.Renderbuffer)
  deriving( Show,Eq,Ord,Data,Typeable,Generic )

createRenderbuffer :: forall a. ImageFormat a => Int -> Int -> Acquire (Renderbuffer a)
createRenderbuffer width height = do
  rbuff <- glResource
  GL.boundRenderbuffer GL.RenderbufferTarget $= rbuff
  glRenderbufferStorage GL_RENDERBUFFER (fromIntegral $ internalFormat (Proxy::Proxy a)) (fromIntegral width) (fromIntegral height)
  return $ Renderbuffer rbuff

resizeRenderbuffer :: MonadIO m => Renderbuffer a -> m (Renderbuffer a)
resizeRenderbuffer rbuff = do
  GL.boundRenderbuffer GL.RenderbufferTarget $= rbuff
  glRenderbufferStorage GL_RENDERBUFFER (fromIntegral $ internalFormat (Proxy::Proxy a)) (fromIntegral width) (fromIntegral height)
  return rbuff
