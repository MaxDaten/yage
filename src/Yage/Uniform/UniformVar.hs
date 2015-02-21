module Yage.Uniform.UniformVar where

import Yage.Prelude
import Quine.StateVar

type UniformVar = SettableStateVar


mkUniformVar :: (a -> IO ()) -> SettableStateVar a
mkUniformVar = SettableStateVar
