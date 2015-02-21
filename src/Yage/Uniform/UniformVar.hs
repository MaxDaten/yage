module Yage.Uniform.UniformVar
 ( UniformVar
 , mkUniformVar
 , toUniformVar
 ) where

import Yage.Prelude
import Quine.StateVar

type UniformVar = SettableStateVar


mkUniformVar :: (a -> IO ()) -> SettableStateVar a
mkUniformVar = SettableStateVar

-- | Converts a 'StateVar' to a just setting 'UniformVar'
toUniformVar :: StateVar a -> UniformVar a
toUniformVar = mkUniformVar.($=)
