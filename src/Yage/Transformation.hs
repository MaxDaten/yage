{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.Transformation where

import           Control.Applicative
import qualified Linear              (lerp, slerp)
import           Yage.Lens
import           Yage.Math           hiding (lerp)
import           Yage.Prelude

data Transformation a = Transformation
    { _transPosition    :: !(V3 a)
    , _transOrientation :: !(Quaternion a)
    , _transScale       :: !(V3 a)
    } deriving ( Show, Eq, Ord, Typeable, Functor )


makeLenses ''Transformation

idTransformation :: RealFloat a => Transformation a
idTransformation = Transformation 0 1 1


transformationMatrix :: Num a => Getter (Transformation a) (M44 a)
transformationMatrix = to get where
    get trans =
        let scaleM       = kronecker . point $ trans^.transScale
            transM       = mkTransformation (trans^.transOrientation) (trans^.transPosition)
        in transM !*! scaleM


inverseTransformation :: (Conjugate a, RealFloat a) => Transformation a -> Transformation a
inverseTransformation t =
    t   & transScale            %~ recip
        & transPosition         %~ negate
        & transOrientation._ijk %~ negate


-- | Creates a Quaternion from a orthonormalized vector space
--   and a normalized look-at direction vector
lookAt :: (Epsilon a, RealFloat a, Show a) => M33 a -> V3 a -> Quaternion a
lookAt (V3 _wx wy wz) directionVector =
    let axis   = normalize $ wy `cross` directionVector
        theta  = wy `dot` directionVector
    in if | nearZero (theta - 1) -> 1
          | nearZero (theta + 1) -> axisAngle wz pi
          | otherwise            -> axisAngle axis (acos theta)
{-# SPECIALISE INLINE lookAt :: M33 Float -> V3 Float -> Quaternion Float #-}
{-# SPECIALISE INLINE lookAt :: M33 Double -> V3 Double -> Quaternion Double #-}


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
    lerp alpha u v = u & transPosition    .~ Linear.lerp (realToFrac alpha) (u^.transPosition) (v^.transPosition)
                       & transScale       .~ Linear.lerp (realToFrac alpha) (u^.transScale) (v^.transScale)
                       & transOrientation .~ Linear.slerp (u^.transOrientation) (v^.transOrientation) (realToFrac alpha)
    {-# INLINE lerp #-}


instance LinearInterpolatable Float where
    lerp alpha u v = (Linear.lerp (realToFrac alpha) (V1 u) (V1 v))^._x
    {-# INLINE lerp #-}

instance LinearInterpolatable Double where
    lerp alpha u v = (Linear.lerp (realToFrac alpha) (V1 u) (V1 v))^._x
    {-# INLINE lerp #-}
