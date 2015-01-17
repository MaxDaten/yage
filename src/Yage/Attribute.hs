module Yage.Attribute
  ( VertexAttribute
  , Layout
  ) where

import Data.Maybe
import Quine.StateVar
import Quine.GL.Attribute

type VertexAttribute = SettableStateVar (Maybe Layout)
