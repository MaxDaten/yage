{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Yage.Uniforms.Material
  ( UniformVar
  , UniformSampler
  , materialUniform
  , materialUniformIntensity
  , materialUniformRGBA
  , materialUniformV4
  -- * Sampler
  , samplerUniform
  , sampler2D
  , samplerCube
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.GL
import           Yage.Material            hiding (over)


import           Quine.GL.Program
import           Quine.GL.Texture         hiding (Texture)
import           Quine.GL.Uniform
import           Quine.GL.Sampler
import           Quine.StateVar
import           Linear

import           Yage.Rendering.Resources.GL.Texture

type UniformVar = SettableStateVar
data UniformSampler = UniformSampler TextureUnit (forall px. UniformVar (Maybe (Texture px)))

samplerUniform :: TextureTarget -> TextureUnit -> Sampler -> UniformSampler
samplerUniform t u s = UniformSampler u $ SettableStateVar $ \mtex -> do
  activeTexture $= u
  boundSampler u $= s
  boundTexture t 0 $= maybe def (view textureObject) mtex

sampler2D :: TextureUnit -> Sampler -> UniformSampler
sampler2D = samplerUniform GL_TEXTURE_2D

samplerCube :: TextureUnit -> Sampler -> UniformSampler
samplerCube = samplerUniform GL_TEXTURE_CUBE_MAP

materialUniformRGBA :: (Functor m, MonadIO m) => Program -> UniformSampler -> String -> String -> m (UniformVar (Material MaterialColorAlpha (Texture px)))
materialUniformRGBA prog sampler texname colorname = contramap (over materialColor linearV4) <$> materialUniformV4 prog sampler texname colorname

materialUniform :: (Functor m, MonadIO m, HasSetter g a IO) => (Program -> UniformLocation -> g) -> Program -> UniformSampler -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniform colorUniform prog (UniformSampler texunit var) texname colorname = do
  texture <- programUniform programUniform1i prog texname
  texture $= fromIntegral texunit
  colorloc <- uniformLocation prog colorname
  return $ SettableStateVar $ \mat -> do
    var $= (Just $ mat^.materialTexture)
    colorUniform prog colorloc $= mat^.materialColor

materialUniformV4 :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler -> String -> String -> m (UniformVar (Material (V4 a) (Texture px)))
materialUniformV4 prog sampler texname colorname = contramap (over (materialColor.mapped) realToFrac) <$> materialUniform programUniform4f prog sampler texname colorname

materialUniformIntensity :: (Functor m, MonadIO m, Real a) => Program -> UniformSampler -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniformIntensity prog sampler texname colorname = contramap (over materialColor realToFrac) <$> materialUniform programUniform1f prog sampler texname colorname
