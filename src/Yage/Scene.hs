{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
module Yage.Scene
    ( module Yage.Scene
    , Cam.rosCamera, Cam.fpsCamera, Cam.camMatrix, CubeMap(..)
    , GLDrawSettings(..), PrimitiveMode(..), Face(..)

    , module Res
    ) where


import           Yage.Prelude hiding (mapM)
import           Yage.Lens

import           Yage.Camera
import           Yage.Light
import           Yage.Resources as Res
import           Yage.Material
import           Yage.Geometry hiding (Face)

import           Data.Traversable (mapM)
import qualified Graphics.GLUtil.Camera3D as Cam

import           Yage.Rendering.Transformation
import           Yage.Rendering.Texture
import           Yage.Rendering.Types hiding (TextureResource)
import           Yage.Rendering hiding (renderData, drawSettings, P3, TextureResource)


data SceneEntity geo = SceneEntity
    { _renderData      :: !(VertexData geo)
    , _textures        :: ![TextureResource]
    , _material        :: !(Material Float)
    , _transformation  :: !(Transformation Float)
    , _drawSettings    :: !GLDrawSettings
    -- , _shader     :: Maybe Shader
    }

makeLenses ''SceneEntity

data SceneLight lit = SceneLight
    { _lightVolume          :: !(TriMesh lit)            -- | currently no file resources supportet (to keep the resource managment simple)
    , _lightTransformation  :: !(Transformation Float)
    , _lightProperties      :: !Light
    , _lightDrawSettings    :: !GLDrawSettings
    }

makeLenses ''SceneLight


data Sky = Sky
    { _skyVolume         :: !(TriMesh P3)
    , _skyTexture        :: !(CubeMap TextureResource)
    , _skyTransformation :: !(Transformation Float)
    , _skyDrawSettings   :: !GLDrawSettings
    , _skyIntensity      :: !Float
    }

makeLenses ''Sky

data Environment lit = Environment
    { _envLights      :: ![lit]
    , _envSky         :: !(Maybe Sky)
    } deriving ( Functor )

makeLenses ''Environment

data Scene ent lit = Scene
    { _sceneEntities    :: [ent]
    , _sceneEnvironment :: Environment lit
    , _sceneCamera      :: Camera
    } deriving ( Functor )

makeLenses ''Scene

type SScene geo lit = Scene (SceneEntity geo) (SceneLight lit)



emptyScene :: Camera -> Scene geo lit
emptyScene = Scene [] (Environment [] Nothing)


addEntity :: SScene geo lit -> SceneEntity geo -> SScene geo lit
addEntity scene r = scene & sceneEntities <>~ [r]


addLight :: SScene geo lit -> SceneLight lit -> SScene geo lit
addLight scene l = scene & sceneEnvironment.envLights <>~ [l]


entitiesCount :: Scene e l -> Int
entitiesCount = length . _sceneEntities

lightCount :: Scene e l -> Int
lightCount = length . _envLights . _sceneEnvironment

sceneSky :: Lens' (Scene ent lit) (Maybe Sky)
sceneSky = sceneEnvironment.envSky

mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera


entityPosition    :: Lens' (SceneEntity v) (Position Float)
entityPosition    = transformation.transPosition

entityScale       :: Lens' (SceneEntity v) (Scale Float)
entityScale       = transformation.transScale

entityOrientation :: Lens' (SceneEntity v) (Orientation Float)
entityOrientation = transformation.transOrientation


skyPosition :: Lens' Sky (Position Float)
skyPosition = skyTransformation.transPosition

--lightPosition    :: Lens' (SceneLight v) (Position Float)
--lightPosition    = lightTransformation.transPosition

--lightScale       :: Lens' (SceneLight v) (Scale Float)
--lightScale       = lightTransformation.transScale

--lightOrientation :: Lens' (SceneLight v) (Orientation Float)
--lightOrientation = lightTransformation.transOrientation



loadSceneEntitiy :: SceneEntity geo -> YageResources geo (SceneEntity geo)
loadSceneEntitiy ent = do
    vdata <- requestVertexData $ ent^.renderData
    eTexs <- mapM requestTextureResource $ ent^.textures
    return $ ent & renderData .~ vdata & textures .~ eTexs


loadSceneResources :: SScene geo lit -> YageResources geo (SScene geo lit)
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


class HasScene a geo lit where
    getScene :: a -> SScene geo lit



instance (ViableVertex (Vertex geo)) => Renderable (SceneEntity geo) geo where
    renderDefinition ent =
        let mesh = either (const emptyMesh) id (ent^.renderData)
            texs = either (error "Yage.Scene.Renderable SceneEntity: missing texture: no default texture defined") (id) <$> (ent^.textures)
            defs = zipWith ($) [TextureDefinition (0, "tex_albedo") . uncurry TextureImage, TextureDefinition (1, "tex_tangent") . uncurry TextureImage] texs
                                
        in RenderEntity mesh (ent^.drawSettings) defs

instance (ViableVertex (Vertex lit)) => Renderable (SceneLight lit) lit where
    renderDefinition lit = RenderEntity (lit^.lightVolume) (lit^.lightDrawSettings) []

instance Renderable Sky P3 where
    renderDefinition sky = 
        let cubeImg = either (error "Yage.Scene.Renderable Sky: missing texture: no default texture defined") id <$> (sky^.skyTexture)
            str     = fst $ cubeFaceRight cubeImg
            tex     = TextureImageCube str (snd <$> cubeImg)
        in RenderEntity (sky^.skyVolume) (sky^.skyDrawSettings) [TextureDefinition (0, "SkyTexture") tex]
