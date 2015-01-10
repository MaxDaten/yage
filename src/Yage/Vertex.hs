{-# OPTIONS_GHC -fno-warn-warnings-deprecations -ddump-splices #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Yage.Vertex
  ( Position(Position)
  , Texture(Texture)
  , Normal(Normal)
  , TangentX(TangentX)
  , TangentY(TangentY)
  , TangentZ(TangentZ)
  , HasPosition(..)
  , HasTexture(..)
  , HasNormal(..)
  , HasTangentX(..)
  , HasTangentY(..)
  , HasTangentZ(..)
  ) where

import Yage.Prelude
import Yage.Lens hiding (coerce)
import Data.Coerce
import Foreign.Ptr
import Foreign.Storable
import Quine.GL.Attribute

newtype Position a  = Position a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)
newtype Texture a   = Texture a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)
newtype Normal a    = Normal a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)
newtype TangentX a = TangentX a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)
newtype TangentY a = TangentY a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)
newtype TangentZ a = TangentZ a deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic,Storable)


class HasPosition a b | a -> b where
  position :: Lens' a b
  positionlayout :: Getter (p a) Layout

class HasTexture a b | a -> b where
  texture :: Lens' a b
  texturelayout :: Getter (p a) Layout

class HasNormal a b | a -> b where
  normal :: Lens' a b
  normallayout :: Getter (p a) Layout

class HasTangentX a b | a -> b where
  tangentX :: Lens' a b
  tangentXlayout :: Getter (p a) Layout

class HasTangentY a b | a -> b where
  tangentY :: Lens' a b
  tangentYlayout :: Getter (p a) Layout

class HasTangentZ a b | a -> b where
  tangentZ :: Lens' a b
  tangentZlayout :: Getter (p a) Layout

instance (Storable a, Attribute a) => HasPosition (Position a) a where
  position = lens coerce (const coerce)
  positionlayout = to $ const $ monoAttribute (Proxy :: Proxy a)

instance (Storable a, Attribute a) => HasNormal (Normal a) a where
  normal = lens coerce (const coerce)
  normallayout = to $ const $ monoAttribute (Proxy :: Proxy a)

instance (Storable a, Attribute a) => HasTexture (Texture a) a where
  texture = lens coerce (const coerce)
  texturelayout = to $ const $ monoAttribute (Proxy :: Proxy a)

instance (Storable a, Attribute a) => HasTangentX (TangentX a) a where
  tangentX = lens coerce (const coerce)
  tangentXlayout = to $ const $ monoAttribute (Proxy :: Proxy a)

instance (Storable a, Attribute a) => HasTangentY (TangentY a) a where
  tangentY = lens coerce (const coerce)
  tangentYlayout = to $ const $ monoAttribute (Proxy :: Proxy a)

instance (Storable a, Attribute a) => HasTangentZ (TangentZ a) a where
  tangentZ = lens coerce (const coerce)
  tangentZlayout = to $ const $ monoAttribute (Proxy :: Proxy a)

monoAttribute :: forall p a. (Attribute a, Storable a) => p a -> Layout
monoAttribute p = Layout (components p) (baseType p) False (sizeOf (undefined::a)) nullPtr
