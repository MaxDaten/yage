{-# LANGUAGE LambdaCase #-}
module Yage.Uniform.Light
  ( lightUniform
  ) where

import           Yage.Prelude
import           Yage.Math
import           Yage.Lens
import           Yage.Light
import           Yage.Transformation
import           Yage.Uniform.UniformVar

import           Quine.GL.Program
import           Quine.GL.Uniform
import           Quine.StateVar


lightUniform :: (MonadIO m, Functor m) => Program -> String -> m (UniformVar Light)
lightUniform prog name = do
  posVar        <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform4f prog $ name++"."++"LightPosition")
  coneAngleVar  <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightConeAnglesAndRadius")
  dirVar        <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightDirection")
  colorVar      <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightColor")
  return $ SettableStateVar $ \light@Light{..} -> do
    colorVar $= _lightColor ^* _lightIntensity
    case _lightType of
      Pointlight{..}        -> do
        posVar        $= light^.position.to point
        coneAngleVar  $= (0 & _z .~ light^.scale._x)
        dirVar        $= 0
      Spotlight{..}         -> do
        posVar        $= light^.position.to point
        coneAngleVar  $= V3 (cos $ _innerAngle / 2) (cos $ _outerAngle / 2) (light^.scale._y)
        dirVar        $= (normalize $ rotate (light^.orientation) (V3 0 1 0))
      DirectionalLight{..}  -> do
        posVar        $= 0
        coneAngleVar  $= 0
        dirVar        $= (normalize $ rotate (light^.orientation) (V3 0 1 0))
