{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Yage.Scene
    ( module Yage.Scene
    , module Yage.Light
    , Cam.rosCamera, Cam.fpsCamera, Cam.camMatrix, Texture(..)
    , GLDrawSettings(..), PrimitiveMode(..), Face(..)

    , module Res
    ) where


import           Yage.Prelude hiding (mapM)
import           Yage.Lens

import           Yage.Camera
import           Yage.Light
import           Yage.Resources as Res
import           Yage.Geometry hiding (Face)

import           Data.Traversable (mapM)
import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Transformation
import           Yage.Rendering.Textures
import           Yage.Rendering.Types
import           Yage.Rendering hiding (renderData, drawSettings, P3)


data SceneEntity vert mat = SceneEntity
    { _renderData      :: !(MeshResource vert)
    , _textures        :: ![TextureResource]
    , _material        :: !mat
    , _transformation  :: !(Transformation Float)
    , _drawSettings    :: !GLDrawSettings
    -- , _shader     :: Maybe Shader
    }

makeLenses ''SceneEntity


data SceneLight lit = SceneLight
    { _lightVolume          :: !(Mesh lit)            -- | currently no file resources supportet (to keep the resource managment simple)
    , _lightTransformation  :: !(Transformation Float)
    , _lightProperties      :: !Light
    , _lightDrawSettings    :: !GLDrawSettings
    }

makeLenses ''SceneLight


data Sky = Sky
    { _skyVolume         :: !(Mesh P3)
    , _skyTexture        :: !(Cube TextureResource)
    , _skyTransformation :: !(Transformation Float)
    , _skyDrawSettings   :: !GLDrawSettings
    , _skyIntensity      :: !Float
    }

makeLenses ''Sky

data Environment lit = Environment
    { _envLights       :: ![lit]
    , _envSky          :: !(Maybe Sky)
    , _envAmbient      :: !AmbientLight
    } deriving ( Functor )

makeLenses ''Environment

data Scene ent lit = Scene
    { _sceneEntities    :: [ent]
    , _sceneEnvironment :: Environment lit
    , _sceneCamera      :: Camera
    } deriving ( Functor )

makeLenses ''Scene

type SScene geoVert geoMat lit = Scene (SceneEntity geoVert geoMat) (SceneLight lit)


emptyScene :: Camera -> Scene geo lit
emptyScene = Scene [] (Environment [] Nothing (AmbientLight 0))


addEntity :: Scene geo lit -> geo -> Scene geo lit
addEntity scene ent = scene & sceneEntities <>~ [ent]


addLight :: Scene geo lit -> lit -> Scene geo lit
addLight scene l = scene & sceneEnvironment.envLights <>~ [l]


entitiesCount :: Scene e l -> Int
entitiesCount = length . _sceneEntities

lightCount :: Scene e l -> Int
lightCount = length . _envLights . _sceneEnvironment

sceneSky :: Lens' (Scene ent lit) (Maybe Sky)
sceneSky = sceneEnvironment.envSky

mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera


entityPosition    :: Lens' (SceneEntity vert mat) (Position Float)
entityPosition    = transformation.transPosition

entityScale       :: Lens' (SceneEntity vert mat) (Scale Float)
entityScale       = transformation.transScale

entityOrientation :: Lens' (SceneEntity vert mat) (Orientation Float)
entityOrientation = transformation.transOrientation


skyPosition :: Lens' Sky (Position Float)
skyPosition = skyTransformation.transPosition

--lightPosition    :: Lens' (SceneLight v) (Position Float)
--lightPosition    = lightTransformation.transPosition

--lightScale       :: Lens' (SceneLight v) (Scale Float)
--lightScale       = lightTransformation.transScale

--lightOrientation :: Lens' (SceneLight v) (Orientation Float)
--lightOrientation = lightTransformation.transOrientation



loadSceneEntitiy :: SceneEntity vert mat -> YageResources vert (SceneEntity vert mat)
loadSceneEntitiy ent = do
    vdata <- requestMeshResource $ ent^.renderData
    eTexs <- mapM requestTextureResource $ ent^.textures
    return $ ent & renderData .~ vdata & textures .~ eTexs


loadSceneResources :: SScene vert mat lit -> YageResources vert (SScene vert mat lit)
loadSceneResources scene =
    Scene <$> (forM (scene^.sceneEntities) loadSceneEntitiy)
          <*> (loadEnvironment $ scene^.sceneEnvironment)
          <*> pure (scene^.sceneCamera) 
    where
        loadEnvironment env = 
            case env^.envSky of
                Nothing -> return env
                Just sky -> do
                    skyTex <- mapM requestTextureResource (sky^.skyTexture)
                    return $ env & envSky %~ over (mapped.skyTexture) (const skyTex)


class HasScene a geoVert geoMat lit where
    getScene :: a -> SScene geoVert geoMat lit



instance (ViableVertex (Vertex geoVert)) => Renderable (SceneEntity geoVert geoMat) geoVert where
    renderDefinition ent =
        let mesh = either (const emptyMesh) id (ent^.renderData)
            texs = either (error "Yage.Scene.Renderable SceneEntity: missing texture: no default texture defined") (id) <$> (ent^.textures)
            defs = zipWith ($) [ TextureDefinition (0, "tex_albedo") . uncurry Texture2D
                               , TextureDefinition (1, "tex_tangent") . uncurry Texture2D
                               ] texs
                                
        in RenderEntity mesh (ent^.drawSettings) defs

instance (ViableVertex (Vertex lit)) => Renderable (SceneLight lit) lit where
    renderDefinition lit = RenderEntity (lit^.lightVolume) (lit^.lightDrawSettings) []

instance Renderable Sky P3 where
    renderDefinition sky = 
        let cubeImg = either (error "Yage.Scene.Renderable Sky: missing texture: no default texture defined") id <$> (sky^.skyTexture)
            str     = fst $ cubeFaceRight cubeImg
            tex     = TextureCube str (snd <$> cubeImg)
        in RenderEntity (sky^.skyVolume) (sky^.skyDrawSettings) [TextureDefinition (0, "SkyTexture") tex]
