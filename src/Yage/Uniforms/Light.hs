{-# LANGUAGE LambdaCase #-}
module Yage.Uniforms.Light
  ( lightUniform
  ) where

import           Yage.Prelude
import           Yage.Math
import           Yage.Lens
import           Yage.Light
import           Yage.Uniforms.UniformVar

import           Quine.GL.Program
import           Quine.GL.Uniform
import           Quine.StateVar


lightUniform :: (MonadIO m, Functor m) => Program -> String -> m (UniformVar Light)
lightUniform prog name = do
  posVar        <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform4f prog $ name++"."++"LightPosition")
  coneAngleVar  <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightConeAnglesAndRadius")
  dirVar        <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightDirection")
  colorVar      <- contramap (fmap realToFrac) . (SettableStateVar.($=)) <$> (programUniform programUniform3f prog $ name++"."++"LightColor")
  return $ SettableStateVar $ \Light{..} -> do
    colorVar $= _lightColor ^* _lightIntensity
    case _lightType of
      Pointlight{..}        -> do
        posVar        $= point _pLightPosition
        coneAngleVar  $= (0 & _z .~ _pLightRadius)
        dirVar        $= 0
      Spotlight{..}         -> do
        posVar        $= point _sLightPosition
        coneAngleVar  $= V3 (cos $ _sLightInnerAngle / 2) (cos $ _sLightOuterAngle / 2) _sLightRadius
        dirVar        $= normalize _sLightDirection
      DirectionalLight{..}  -> do
        posVar        $= 0
        coneAngleVar  $= 0
        dirVar        $= _dLightDirection
