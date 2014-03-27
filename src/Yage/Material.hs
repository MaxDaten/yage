{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor   #-}
module Yage.Material where

import Yage.Prelude
import Yage.Lens
import Linear

data Material a = Material
    { _matBaseColor    :: !(V4 a)
    , _matSpecular     :: !a
    } deriving ( Show, Eq, Ord, Functor )

makeLenses ''Material
