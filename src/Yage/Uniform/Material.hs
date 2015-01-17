{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Yage.Uniform.Material
  ( UniformVar
  , UniformSampler
  , materialUniform
  , materialUniformIntensity
  , materialUniformRGBA
  , materialUniformV4
  -- * Sampler
  , sampler
  , sampler2D
  , samplerCube
  , samplerUniform
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
import Quine.StateVar
import Linear

import Yage.Rendering.Resources.GL.Texture

data UniformSampler px = UniformSampler TextureUnit (UniformVar (Maybe (Texture px)))

instance MonadIO m => HasSetter (UniformSampler px) (Maybe (Texture px)) m where
  (UniformSampler _ s) $= t = s $= t

sampler :: TextureTarget -> TextureUnit -> Sampler -> UniformSampler px
sampler t u s = UniformSampler u $ SettableStateVar $ \mtex -> do
  traverse_ (\tex -> when (tex^.textureTarget /= t) $ error "TextureTarget mismatch would result in GL error") mtex
  activeTexture $= u
  boundSampler u $= s
  boundTexture t 0 $= maybe def (view textureObject) mtex

sampler2D :: TextureUnit -> Sampler -> UniformSampler px
sampler2D = sampler GL_TEXTURE_2D

samplerCube :: TextureUnit -> Sampler -> UniformSampler px
samplerCube = sampler GL_TEXTURE_CUBE_MAP

samplerUniform :: MonadIO m => Program -> UniformSampler px -> String -> m (UniformVar (Maybe (Texture px)))
samplerUniform prog (UniformSampler texunit var) texname = do
  texture <- programUniform programUniform1i prog texname
  texture $= fromIntegral texunit
  return $ SettableStateVar $ \tex -> var $= tex

materialUniformRGBA :: (Functor m, MonadIO m) => Program -> UniformSampler px  -> String -> String -> m (UniformVar (Material MaterialColorAlpha (Texture px)))
materialUniformRGBA prog smpl texname colorname = contramap (over materialColor linearV4) <$> materialUniformV4 prog smpl texname colorname

materialUniform :: (Functor m, MonadIO m, HasSetter g a IO) => (Program -> UniformLocation -> g) -> Program -> UniformSampler px  -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniform colorUniform prog (UniformSampler texunit var) texname colorname = do
  texture <- programUniform programUniform1i prog texname
  texture $= fromIntegral texunit
  colorloc <- uniformLocation prog colorname
  return $ SettableStateVar $ \mat -> do
    var $= (Just $ mat^.materialTexture)
    colorUniform prog colorloc $= mat^.materialColor

materialUniformV4 :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler px  -> String -> String -> m (UniformVar (Material (V4 a) (Texture px)))
materialUniformV4 prog smpl texname colorname = contramap (over (materialColor.mapped) realToFrac) <$> materialUniform programUniform4f prog smpl texname colorname

materialUniformIntensity :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler px  -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniformIntensity prog smpl texname colorname = contramap (over materialColor realToFrac) <$> materialUniform programUniform1f prog smpl texname colorname
