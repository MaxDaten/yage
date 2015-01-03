{-# LANGUAGE FlexibleContexts #-}
module Yage.Uniforms.Material
  ( UniformVar
  , materialUniformColor
  , materialUniform
  , materialUniformColor1
  , materialUniformColor4
  ) where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Material            hiding (over)


import           Quine.GL.Program
import           Quine.GL.Texture         hiding (Texture)
import           Quine.GL.Uniform
import           Quine.StateVar
import           Linear

import           Yage.Rendering.Resources.GL.Texture

type UniformVar = SettableStateVar

materialUniformColor :: (Functor m, MonadIO m) => Program -> TextureUnit -> String -> String -> m (UniformVar (Material MaterialColorAlpha (Texture px)))
materialUniformColor prog texunit texname colorname = contramap (over materialColor linearV4) <$> materialUniformColor4 prog texunit texname colorname

materialUniform :: (Functor m, MonadIO m, HasSetter g a IO) => (Program -> UniformLocation -> g) -> Program -> TextureUnit -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniform colorUniform prog texunit texname colorname = do
  texture <- programUniform programUniform1i prog texname
  texture $= (fromIntegral texunit)
  colorloc <- uniformLocation prog colorname
  return $ SettableStateVar $ \mat -> do
    bindTextures (mat^.materialTexture.textureTarget) [(texunit, Just $ mat^.materialTexture)]
    colorUniform prog colorloc $= mat^.materialColor

materialUniformColor4 :: (Functor m, MonadIO m, Real a) => Program -> TextureUnit -> String -> String -> m (UniformVar (Material (V4 a) (Texture px)))
materialUniformColor4 prog texunit texname colorname = contramap (over (materialColor.mapped) realToFrac) <$> materialUniform programUniform4f prog texunit texname colorname

materialUniformColor1 :: (Functor m, MonadIO m, Real a) => Program -> TextureUnit -> String -> String -> m (UniformVar (Material a (Texture px)))
materialUniformColor1 prog texunit texname colorname = contramap (over materialColor realToFrac) <$> materialUniform programUniform1f prog texunit texname colorname
