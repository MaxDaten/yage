{-# LANGUAGE TemplateHaskell #-}
module Yage.Viewport
    ( module Yage.Viewport
    ) where

import Yage.Prelude
import Yage.Math
import Yage.Lens

import Yage.Core.OpenGL as GL (Position(..), Size(..))
import qualified Graphics.GLUtil.Camera3D            as Cam



data Viewport a = Viewport
    { _viewportXY     :: V2 a
    , _viewportWH     :: V2 a       -- ^ (width, height) in px
    , _viewportGamma  :: Float
    } 
    deriving ( Typeable, Functor, Show, Eq, Ord , Generic )

makeLenses ''Viewport


glViewport :: Getter (Viewport Int) (GL.Position, GL.Size)
glViewport = to get where
    get vp' = 
        let vp  = fromIntegral <$> vp'
        in ( GL.Position ( vp^.viewportXY._x ) ( vp^.viewportXY._y )
           , GL.Size     ( vp^.viewportWH._x ) ( vp^.viewportWH._y )
           )


-- | creates the projectiom matrix for the given viewport
-- for Camera2D: create an orthographic matrix with origin at the
-- top left corner of the screen
-- for Camera3D: creates a perspective projection matrix 
projectionMatrix3D :: (Conjugate a, Epsilon a, RealFloat a) => a -> a -> a -> Viewport a -> M44 a
projectionMatrix3D zNear zFar fov vp = Cam.projectionMatrix
        ( realToFrac fov )
        ( (vp^.viewportWH._x) / (vp^.viewportWH._y) )
        ( realToFrac $ zNear )
        ( realToFrac $ zFar )

    --orthographicMatrix -- 0/0 top left
    --    ( viewport^.vpXY._x ) 
    --    ( viewport^.vpXY._x + viewport^.vpSize._x )
    --    ( viewport^.vpXY._y )
    --    ( viewport^.vpXY._y + viewport^.vpSize._y )
    --    ( realToFrac $ zNear )
    --    ( realToFrac $ zFar )

projectionMatrix2D :: (Conjugate a, Epsilon a, RealFloat a) => 
    a -> 
    -- ^ zNear
    a -> 
    -- ^ zFar
    Viewport a ->
    -- ^ viewport 
    M44 a
    -- ^ projection matrix
projectionMatrix2D zNear zFar viewport =
    orthographicMatrix -- 0/0 top left
        ( viewport^.viewportXY._x ) 
        ( viewport^.viewportXY._x + viewport^.viewportWH._x )
        ( viewport^.viewportXY._y )
        ( viewport^.viewportXY._y + viewport^.viewportWH._y )
        ( realToFrac $ zNear )
        ( realToFrac $ zFar )

orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                    => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r t b n f = 
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )
