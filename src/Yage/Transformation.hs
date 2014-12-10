{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Yage.Transformation where

import           Yage.Prelude
import           Yage.Lens
import           Yage.Math           hiding (lerp)

import           Control.Applicative
import           Data.Data
import qualified Linear              (lerp, slerp)

data Transformation a = Transformation
  { _transformationPosition    :: !(V3 a)
  , _transformationOrientation :: !(Quaternion a)
  , _transformationScale       :: !(V3 a)
  } deriving (Show,Eq,Ord,Functor,Traversable,Foldable,Data,Typeable,Generic)


-- makeLenses ''Transformation
makeFields ''Transformation
makeClassy ''Transformation

idTransformation :: RealFloat a => Transformation a
idTransformation = Transformation 0 1 1


transformationMatrix :: (Num a, HasPosition t (V3 a), HasScale t (V3 a), HasOrientation t (Quaternion a)) => Getter t (M44 a)
transformationMatrix = to get where
  get trans =
    let scaleM       = kronecker . point $ trans^.scale
        transM       = mkTransformation (trans^.orientation) (trans^.position)
    in transM !*! scaleM


inverseTransformation :: (Conjugate a, RealFloat a, HasTransformation t a) => t -> t
inverseTransformation t =
  t & transformation.scale            %~ recip
    & transformation.position         %~ negate
    & transformation.orientation._ijk %~ negate


instance Applicative Transformation where
  pure a = Transformation (pure a) (pure a) (pure a)
  (Transformation fp fo fs) <*> (Transformation p o s) = Transformation (fp <*> p) (fo <*> o) (fs <*> s)

instance RealFloat a => Num (Transformation a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger


instance (RealFloat a, Epsilon a) => Epsilon (Transformation a) where
  nearZero (Transformation p o s) = nearZero p && nearZero o && nearZero s

instance RealFloat a => Default (Transformation a) where
  def = idTransformation

class LinearInterpolatable a where
  lerp :: Double -> a -> a -> a

instance RealFloat a => LinearInterpolatable (Transformation a) where
  lerp alpha u v = u & position     .~ Linear.lerp (realToFrac alpha) (u^.position) (v^.position)
                     & scale        .~ Linear.lerp (realToFrac alpha) (u^.scale) (v^.scale)
                     & orientation  .~ Linear.slerp (u^.orientation) (v^.orientation) (realToFrac alpha)
  {-# INLINE lerp #-}

instance LinearInterpolatable Float where
  lerp alpha u v = (Linear.lerp (realToFrac alpha) (V1 u) (V1 v))^._x
  {-# INLINE lerp #-}

instance LinearInterpolatable Double where
  lerp alpha u v = (Linear.lerp (realToFrac alpha) (V1 u) (V1 v))^._x
  {-# INLINE lerp #-}
