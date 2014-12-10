{-# OPTIONS_GHC -fno-warn-name-shadowing        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
module Yage.Viewport
    ( module Yage.Viewport
    , module Rectangle
    ) where

import Yage.Prelude
import Yage.Math
import Yage.Lens
import Yage.Geometry.D2.Rectangle as Rectangle (Rectangle(..), GetRectangle(..), HasRectangle(..), rectangle, asRectangle, extend)

data Viewport a = Viewport
    { _viewportRect       :: Rectangle a
      -- ^ as xy1 und xy2, with 0/0 top/left
    , _viewportPixelRatio :: V2 Double
    -- ^ usually 1:1, on retina displays 2:2
    , _viewportGamma      :: Float
    }
    deriving ( Typeable, Functor, Show, Eq, Generic )

makeLenses ''Viewport


instance HasRectangle (Viewport Int) Int where
    rectangle = viewportRect
    {-# INLINE rectangle #-}


glViewport :: HasRectangle t Int => Getter t (GL.Position, GL.Size)
glViewport = rectangle.to get where
    get :: Rectangle Int -> (GL.Position, GL.Size)
    get rect@( Rectangle (V2 x y) _ ) =
        let (V2 w h) = rect^.extend
        in ( GL.Position ( fromIntegral $ x ) ( fromIntegral $ y )
           , GL.Size     ( fromIntegral $ w ) ( fromIntegral $ h )
           )



-- | creates the projectiom matrix for the given viewport
-- for Camera2D: create an orthographic matrix with origin at the
-- top left corner of the screen
-- for Camera3D: creates a perspective projection matrix
projectionMatrix3D :: (Conjugate a, Epsilon a, RealFloat a) => a -> a -> a -> Rectangle a -> M44 a
projectionMatrix3D zNear zFar fov (Rectangle _ wh) = Cam.projectionMatrix
        ( realToFrac fov )
        ( wh^._x / wh^._y )
        ( realToFrac $ zNear )
        ( realToFrac $ zFar )


-- | glOrtho convention
orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                   => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r b t n f =
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )

