{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yage.Uniform.Image
  ( ImageUnit
  , ImageTexture
  , ImageTexture1D
  , ImageTexture2D
  , ImageTexture3D
  , ImageTextureCube
  -- * State Vars
  , imageTexture
  , imageTextureUniform
  ) where

import Yage.Prelude
import Yage.Lens
import Yage.GL
import Yage.Material            hiding (over)
import Yage.Uniform.UniformVar

import Data.Foldable
import Quine.GL.Program
import Quine.GL.Texture         hiding (Texture)
import Quine.GL.Uniform
import Quine.GL.Sampler
import Quine.GL.Object
import Quine.StateVar
import Linear

import Graphics.GL.Ext.ARB.ShaderImageLoadStore
import Graphics.GL.Types
import Yage.Rendering.Resources.GL.Texture

type ImageUnit = Word32
type ImageAccess = GLenum
data ImageTexture d px = ImageTexture ImageUnit (UniformVar (Maybe (Texture d px)))

type ImageTexture1D   = ImageTexture Tex1D
type ImageTexture2D   = ImageTexture Tex2D
type ImageTexture3D   = ImageTexture Tex3D
type ImageTextureCube = ImageTexture TexCube

instance MonadIO m => HasSetter (ImageTexture d px) (Maybe (Texture d px)) m where
  (ImageTexture _ s) $= t = s $= t

imageTexture :: forall d px. ImageFormat px => ImageUnit -> ImageAccess -> ImageTexture d px
imageTexture u access = ImageTexture u $ SettableStateVar $ \mtex -> do
  let tex = maybe def (view textureObject) mtex
  glBindImageTexture u (object tex) 0 GL_TRUE 0 access (internalFormat (Proxy :: Proxy px))

imageTextureUniform :: MonadIO m => Program -> ImageTexture d px -> String -> m (UniformVar (Maybe (Texture d px)))
imageTextureUniform prog (ImageTexture imgUnit bindImg) name = do
  imgUniform <- programUniform programUniform1i prog name
  return $ SettableStateVar $ \tex -> do
    bindImg $= tex
    imgUniform $= fromIntegral imgUnit

