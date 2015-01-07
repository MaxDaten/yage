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
-- import Quine.GL.Attribute (Layout)

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

-- | current implementation example
-- FIXME: major problem here is, it violates the first lens law (you get what you set). you cant set currently any layout, this could be missleading
-- subject for a rework
{--
instance HasPosition YGMVertexLayout Layout where
  position = lens (const $ Layout 3 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr)) (const)
instance HasTexture (HasLayout YGMVertex) Layout where
  texture = lens (const $ Layout 2 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3)))) (const)
instance HasTangentX (HasLayout YGMVertex) Layout where
  tangentX = lens (const $ Layout 3 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2)))) (const)
instance HasTangentZ (HasLayout YGMVertex) Layout where
  tangentZ = lens (const $ Layout 4 GL_FLOAT False (sizeOf (undefined::YGMVertex)) (nullPtr `plusPtr` (sizeOf (undefined::Vec3) + sizeOf (undefined::Vec2) + sizeOf (undefined::Vec3)))) (const)
--}
