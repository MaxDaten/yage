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
import           Yage.Lens

import           Yage.Camera
import           Yage.Light
import           Yage.Resources


import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Transformation
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

data SceneLight lit = SceneLight
    { _lightVolume          :: !(TriMesh lit)            -- | currently no file resources supportet (to keep the resource managment simple)
    , _lightTransformation  :: !(Transformation Float)
    , _lightProperties      :: !Light
    }

makeLenses ''SceneLight

data Scene ent lit = Scene
    { _sceneEntities :: [ent]
    , _sceneLights   :: [lit]
    , _sceneCamera   :: Camera
    } deriving ( Functor )

makeLenses ''Scene

type SScene geo lit = Scene (SceneEntity geo) (SceneLight lit)



emptyScene :: Camera -> Scene geo lit
emptyScene = Scene [] []


addEntity :: SScene geo lit -> SceneEntity geo -> SScene geo lit
addEntity scene r = scene & sceneEntities <>~ [r]


addLight :: SScene geo lit -> SceneLight lit -> SScene geo lit
addLight scene l = scene & sceneLights <>~ [l]


entitiesCount :: Scene e l -> Int
entitiesCount = length . _sceneEntities

lightCount :: Scene e l -> Int
lightCount = length . _sceneLights


mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera


entityPosition    :: Lens' (SceneEntity v) (Position Float)
entityPosition    = transformation.transPosition

entityScale       :: Lens' (SceneEntity v) (Scale Float)
entityScale       = transformation.transScale

entityOrientation :: Lens' (SceneEntity v) (Orientation Float)
entityOrientation = transformation.transOrientation


--lightPosition    :: Lens' (SceneLight v) (Position Float)
--lightPosition    = lightTransformation.transPosition

--lightScale       :: Lens' (SceneLight v) (Scale Float)
--lightScale       = lightTransformation.transScale

--lightOrientation :: Lens' (SceneLight v) (Orientation Float)
--lightOrientation = lightTransformation.transOrientation



loadSceneEntitiy :: SceneEntity geo -> YageResources geo (SceneEntity geo)
loadSceneEntitiy ent = do
    vdata <- requestVertexData $ ent^.renderData
    return $ ent & renderData .~ vdata


loadSceneResources :: SScene geo lit -> YageResources geo (SScene geo lit)
loadSceneResources scene = 
    Scene <$> (forM (scene^.sceneEntities) loadSceneEntitiy)
          <*> pure (scene^.sceneLights)
          <*> pure (scene^.sceneCamera) 



class HasScene a geo lit where
    getScene :: a -> SScene geo lit



instance (ViableVertex (Vertex geo)) => Renderable (SceneEntity geo) geo where
    renderDefinition ent =
        let mesh = either (const emptyMesh) id (ent^.renderData)
        in RenderEntity mesh (ent^.renderMode) (ent^.textures)

instance (ViableVertex (Vertex lit)) => Renderable (SceneLight lit) lit where
    renderDefinition lit = RenderEntity (lit^.lightVolume) Triangles []
