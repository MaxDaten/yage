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

import Yage.Core.OpenGL as GL (Position(..), Size(..))
import qualified Graphics.GLUtil.Camera3D            as Cam



data Viewport a = Viewport
    { _viewportRect   :: Rectangle a
      -- ^ as container for xy and wh, with 0/0 top/left
    , _viewportGamma  :: Float
    } 
    deriving ( Typeable, Functor, Show, Eq, Generic )

makeLenses ''Viewport


instance HasRectangle (Viewport Int) Int where
    rectangle = viewportRect
    {-# INLINE rectangle #-}


glViewport :: HasRectangle t Int => Getter t (GL.Position, GL.Size)
glViewport = rectangle.to get where
    get :: Rectangle Int -> (GL.Position, GL.Size)
    get (Rectangle xy wh) = 
        ( GL.Position ( fromIntegral $ xy^._x ) ( fromIntegral $ xy^._y )
        , GL.Size     ( fromIntegral $ wh^._x ) ( fromIntegral $ wh^._y )
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


projectionMatrix2D :: (Conjugate a, Epsilon a, RealFloat a) => 
    a -> 
    -- ^ zNear
    a -> 
    -- ^ zFar
    Rectangle a ->
    -- ^ xy wh
    M44 a
    -- ^ projection matrix
projectionMatrix2D zNear zFar (Rectangle xy wh) =
    orthographicMatrix -- 0/0 top left
        ( xy^._x ) 
        ( xy^._x + wh^._x )
        ( xy^._y )
        ( xy^._y + wh^._y )
        ( realToFrac $ zNear )
        ( realToFrac $ zFar )


orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                    => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r t b n f = 
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )

