{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Yage.Vertex
  ( HasLayout(HasLayout)
  , Position(Position)
  , HasPosition(position)
  , Texture(Texture)
  , HasTexture(texture)
  , Normal(Normal)
  , HasNormal(normal)
  , Tangent(Tangent)
  , HasTangentX(tangentX)
  , HasTangentY(tangentY)
  , HasTangentZ(tangentZ)
  ) where

import Yage.Prelude
import Yage.Lens
import Foreign.Ptr
import Foreign.Storable
import Quine.GL.Attribute

-- | 'HasLayout' is just a type level proxy for vertex data objects.
-- It allows the annotation of a OpenGL compatible 'Layout' to the
-- field accessors of a concrete defined vertex data type
data HasLayout a = HasLayout

newtype Position a  = Position { _positionPosition :: a } deriving (Eq,Ord,Show,Generic,Storable)
newtype Texture a   = Texture { _textureTexture :: a } deriving (Eq,Ord,Show,Generic,Storable)
newtype Normal a    = Normal { _normalNormal :: a } deriving (Eq,Ord,Show,Generic,Storable)
data Tangent a = Tangent
  { _tangentTangentX :: a
  , _tangentTangentY :: a
  , _tangentTangentZ :: a
  }


makeFields ''Position
makeFields ''Texture
makeFields ''Normal
makeFields ''Tangent

instance (Storable a, Attribute a) => HasPosition (HasLayout (Position a)) Layout where
  position = lens (const $ Layout (components (Proxy::Proxy a)) (baseType (Proxy::Proxy a)) False (sizeOf (undefined::a)) (nullPtr)) (const)

instance (Storable a, Attribute a) => HasTexture (HasLayout (Texture a)) Layout where
  texture = lens (const $ Layout (components (Proxy::Proxy a)) (baseType (Proxy::Proxy a)) False (sizeOf (undefined::a)) (nullPtr)) (const)

instance (Storable a, Attribute a) => HasNormal (HasLayout (Normal a)) Layout where
  normal = lens (const $ Layout (components (Proxy::Proxy a)) (baseType (Proxy::Proxy a)) False (sizeOf (undefined::a)) (nullPtr)) (const)

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
