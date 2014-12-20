{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Yage.Rendering.Resources.GL.Renderbuffer
  ( Renderbuffer
  , renderbufferDimension
  , renderbufferGL
  -- * Creation
  , createRenderbuffer
  -- * Resize
  , resizeRenderbuffer
  -- * Format
  , module TextureFormat
  -- * Handy Aliases
  , RenderbufferD24F
  , RenderbufferD32F
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math
import           Yage.Rendering.GL
import           Yage.Resources

import           Data.Data
import qualified Quine.GL.Renderbuffer as GL
import           Quine.GL.Framebuffer
import           Quine.Image
import           Quine.StateVar
import           Yage.Rendering.Resources.GL.Base
import           Yage.Rendering.Resources.GL.TextureFormat as TextureFormat

data Renderbuffer a = Renderbuffer
  { _renderbufferDimension :: V2 Int
  , _renderbufferGL        :: (GL.Renderbuffer a)
  } deriving (Typeable)

type RenderbufferD24F = Renderbuffer (DepthComponent24 Float)
type RenderbufferD32F = Renderbuffer (DepthComponent32 Float)

makeLenses ''Renderbuffer

createRenderbuffer :: ImageFormat a => Int -> Int -> Acquire (Renderbuffer a)
createRenderbuffer width height = do
  rbuff <- glResource
  resizeRenderbuffer (Renderbuffer (V2 width height) rbuff) width height

resizeRenderbuffer :: forall a m. (ImageFormat a, MonadIO m) => Renderbuffer a -> Int -> Int -> m (Renderbuffer a)
resizeRenderbuffer (Renderbuffer _ rbuff) width height = do
  GL.boundRenderbuffer GL.RenderbufferTarget $= rbuff
  glRenderbufferStorage GL_RENDERBUFFER (fromIntegral $ internalFormat (Proxy::Proxy a)) (fromIntegral width) (fromIntegral height)
  return $ Renderbuffer (V2 width height) rbuff

instance FramebufferAttachment (Renderbuffer a) where
  attach target slot = attach target slot . view renderbufferGL
