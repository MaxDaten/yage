{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Yage.Scene
    ( module Yage.Scene
    , Cam.rosCamera, Cam.fpsCamera, Cam.camMatrix
    ) where


import           Yage.Prelude
import           Yage.Camera
import           Yage.Resources

import           Control.Monad

import           Data.List (length)

import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Transformation
import           Yage.Rendering.Viewport
import           Yage.Rendering.Types
import           Yage.Rendering hiding (renderData, renderMode)


data SceneEntity geo = SceneEntity
    { _renderData      :: !(VertexData geo)
    , _renderMode      :: !PrimitiveMode
    , _textures        :: [TextureDefinition]
    , _transformation  :: !(Transformation Float)
    -- , _shader     :: Maybe Shader
    }

makeLenses ''SceneEntity

data Scene ent = Scene
    { _sceneEntities :: [ent]
    , _sceneCamera   :: Camera
    } deriving ( Functor )

makeLenses ''Scene

type SScene geo = Scene (SceneEntity geo)
type RScene geo = Scene (RenderEntity geo)

data SceneView geo = SceneView (RScene geo) ViewportI


emptyScene :: Camera -> Scene geo
emptyScene = Scene []


addEntity :: SScene geo -> SceneEntity geo -> SScene geo
addEntity scene r = scene & sceneEntities <>~ [r]


entitiesCount :: Scene e -> Int
entitiesCount = length . _sceneEntities


mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera




entityPosition    :: Lens' (SceneEntity v) (Position Float)
entityPosition    = transformation.transPosition

entityScale       :: Lens' (SceneEntity v) (Scale Float)
entityScale       = transformation.transScale

entityOrientation :: Lens' (SceneEntity v) (Orientation Float)
entityOrientation = transformation.transOrientation



loadSceneEntitiy :: SceneEntity geo -> YageResources geo (SceneEntity geo)
loadSceneEntitiy ent = do
    vdata <- requestVertexData $ ent^.renderData
    return $ ent & renderData .~ vdata


loadSceneResources :: SScene geo -> YageResources geo (SScene geo)
loadSceneResources scene = Scene <$> (mapM loadSceneEntitiy $ scene^.sceneEntities)
                                 <*> pure (scene^.sceneCamera) 



class HasScene a geo where
    getScene :: a -> SScene geo


-- | creates the projectiom matrix for the given viewport
-- for Camera2D: create an orthographic matrix with origin at the
-- top left corner of the screen
-- for Camera3D: creates a perspective projection matrix 
cameraProjectionMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                       => Camera -> Viewport a -> M44 a
cameraProjectionMatrix (Camera2D _ planes )     v =
    orthographicMatrix -- 0/0 top left
        ( v^.vpXY._x ) 
        ( v^.vpXY._x + v^.vpSize._x )
        ( v^.vpXY._y )
        ( v^.vpXY._y + v^.vpSize._y )
        ( realToFrac $ planes^.camZNear )
        ( realToFrac $ planes^.camZFar )
cameraProjectionMatrix (Camera3D _ planes fov ) v = 
    Cam.projectionMatrix
        ( realToFrac $ fov )
        ( (v^.vpSize._x) / (v^.vpSize._y) )
        ( realToFrac $ planes^.camZNear )
        ( realToFrac $ planes^.camZFar )

orthographicMatrix :: (Conjugate a, Epsilon a, RealFloat a)
                    => a -> a -> a -> a -> a -> a -> M44 a
orthographicMatrix l r t b n f = 
    V4 ( V4 (2/(r-l)) 0        0             (-(r+l)/(r-l)) )
       ( V4 0        (2/(t-b)) 0             (-(t+b)/(t-b)) )
       ( V4 0        0         ((-2)/(f-n))  (-(f+n)/(f-n)) )
       ( V4 0        0         0             1              )


instance (ViableVertex (Vertex geo)) => Renderable (SceneEntity geo) geo where
    renderDefinition ent =
        let mesh = either (const emptyMesh) (id) (ent^.renderData)
        in RenderEntity mesh (ent^.renderMode) (ent^.textures)

