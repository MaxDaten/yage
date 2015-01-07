{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Yage.Vertex
  ( HasLayout(HasLayout)
  , HasPosition(position)
  , HasTexture(texture)
  , HasNormal(normal)
  , HasTangentX(tangentX)
  , HasTangentY(tangentY)
  , HasTangentZ(tangentZ)
  ) where

import Yage.Lens

-- | 'HasLayout' is just a type level proxy for vertex data objects.
-- It allows the annotation of a OpenGL compatible 'Layout' to the
-- field accessors of a concrete defined vertex data type
data HasLayout a = HasLayout

newtype Position a = Position { _positionPosition :: a }
newtype Texture a = Texture { _textureTexture :: a }
newtype Normal a = Normal { _normalNormal :: a }
data Tangent a = Tangent
  { _tangentTangentX :: a
  , _tangentTangentY :: a
  , _tangentTangentZ :: a
  }


makeFields ''Position
makeFields ''Texture
makeFields ''Normal
makeFields ''Tangent
