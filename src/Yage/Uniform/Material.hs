{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Yage.Uniform.Material
  ( UniformVar
  , UniformSampler
  , UniformSampler1D
  , UniformSampler2D
  , UniformSampler3D
  , UniformSamplerCube
  , materialUniform
  , materialUniformIntensity
  , materialUniformRGBA
  , materialUniformV4
  -- * Sampler
  , sampler
  , sampler2D
  , sampler3D
  , samplerCube
  , samplerUniform
  -- * Reexports
  , TextureUnit
  , module Sampler
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
import Quine.GL.Sampler         as Sampler
import Quine.StateVar
import Linear

import Yage.Rendering.Resources.GL.Texture

data UniformSampler d px = UniformSampler TextureUnit (UniformVar (Maybe (Texture d px)))

type UniformSampler1D   = UniformSampler Tex1D
type UniformSampler2D   = UniformSampler Tex2D
type UniformSampler3D   = UniformSampler Tex3D
type UniformSamplerCube = UniformSampler TexCube

instance MonadIO m => HasSetter (UniformSampler d px) (Maybe (Texture d px)) m where
  (UniformSampler _ s) $= t = s $= t

sampler :: TextureTarget -> TextureUnit -> Sampler -> UniformSampler d px
sampler t u s = UniformSampler u $ SettableStateVar $ \mtex -> do
  traverse_ (\tex -> when (tex^.textureTarget /= t) $ error "TextureTarget mismatch would result in GL error") mtex
  activeTexture $= u
  boundTexture t 0 $= maybe def (view textureObject) mtex
  boundSampler u $= s

sampler2D :: TextureUnit -> Sampler -> UniformSampler2D px
sampler2D = sampler GL_TEXTURE_2D

sampler3D :: TextureUnit -> Sampler -> UniformSampler3D px
sampler3D = sampler GL_TEXTURE_3D

samplerCube :: TextureUnit -> Sampler -> UniformSamplerCube px
samplerCube = sampler GL_TEXTURE_CUBE_MAP

samplerUniform :: MonadIO m => Program -> UniformSampler d px -> String -> m (UniformVar (Maybe (Texture d px)))
samplerUniform prog s@(UniformSampler texunit var) texname = do
  texture <- programUniform programUniform1i prog texname
  texture $= fromIntegral texunit
  return $ SettableStateVar $ \tex -> s $= tex

materialUniformRGBA :: (Functor m, MonadIO m) => Program -> UniformSampler d px  -> String -> String -> m (UniformVar (Material MaterialColorAlpha (Texture d px)))
materialUniformRGBA prog smpl texname colorname = contramap (over materialColor linearV4) <$> materialUniformV4 prog smpl texname colorname

materialUniform :: (Functor m, MonadIO m, HasSetter g a IO) => (Program -> UniformLocation -> g) -> Program -> UniformSampler d px  -> String -> String -> m (UniformVar (Material a (Texture d px)))
materialUniform colorUniform prog (UniformSampler texunit var) texname colorname = do
  texture <- programUniform programUniform1i prog texname
  texture $= fromIntegral texunit
  colorloc <- uniformLocation prog colorname
  return $ SettableStateVar $ \mat -> do
    var $= (Just $ mat^.materialTexture)
    colorUniform prog colorloc $= mat^.materialColor

materialUniformV4 :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler d px  -> String -> String -> m (UniformVar (Material (V4 a) (Texture d px)))
materialUniformV4 prog smpl texname colorname = contramap (over (materialColor.mapped) realToFrac) <$> materialUniform programUniform4f prog smpl texname colorname

materialUniformIntensity :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler d px  -> String -> String -> m (UniformVar (Material a (Texture d px)))
materialUniformIntensity prog smpl texname colorname = contramap (over materialColor realToFrac) <$> materialUniform programUniform1f prog smpl texname colorname
