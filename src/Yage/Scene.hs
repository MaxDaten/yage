{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
module Yage.Scene
    ( module Yage.Scene
    , module Yage.Light
    , Cam.rosCamera, Cam.fpsCamera, Cam.camMatrix, Texture(..)
    , GLDrawSettings(..)

    , module Res
    ) where


import           Yage.Prelude                   hiding (mapM)
import           Yage.Lens

import           Yage.Camera
import           Yage.Light
import           Yage.Resources                 as Res
import           Yage.Geometry                  hiding (Face)

import           Data.Traversable               (mapM)
import qualified Graphics.GLUtil.Camera3D       as Cam

import           Yage.Rendering.Transformation
import           Yage.Rendering.RenderEntity
import           Yage.Rendering                 hiding (P3)


data Entity mesh mat = Entity
    { _renderData      :: !mesh
    , _materials       :: !mat
    , _transformation  :: !( Transformation Float )
    , _drawSettings    :: !GLDrawSettings
    }

makeLenses ''Entity

data SceneLight mesh = SceneLight (Entity mesh ()) !Light

type Sky mat = Entity (Mesh P3) mat

data Environment lit mat = Environment
    { _envLights       :: ![ lit ]
    , _envSky          :: !( Maybe ( Sky mat ) )
    , _envAmbient      :: !AmbientLight
    }

makeLenses ''Environment



data Scene ent env = Scene
    { _sceneEntities    :: [ ent ]
    , _sceneEnvironment :: env
    , _sceneCamera      :: Camera
    } deriving ( Functor )

makeLenses ''Scene


--type ResScene geoVert geoMat litVert skyMat = 
--    Scene 
--        (Entity (MeshResource geoVert) geoMat)
--        (Environment (SceneLight litVert) skyMat)


emptyScene :: Camera -> Scene ent (Environment lit skymat)
emptyScene = Scene [] emptyEnvironment


emptyEnvironment :: Environment lit mat
emptyEnvironment = Environment [] Nothing (AmbientLight 0)


addEntity :: Scene ent env -> ent -> Scene ent env
addEntity scene ent = scene & sceneEntities <>~ [ent]


addLight :: Scene ent (Environment lit mat) -> lit -> Scene ent (Environment lit mat)
addLight scene l = scene & sceneEnvironment.envLights <>~ [l]


entitiesCount :: Scene ent (Environment l m) -> Int
entitiesCount = length . _sceneEntities

lightCount :: Scene ent (Environment l m) -> Int
lightCount = length . _envLights . _sceneEnvironment

sceneSky :: Lens' (Scene ent (Environment lit m)) (Maybe (Sky m))
sceneSky = sceneEnvironment.envSky

mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera


entityPosition    :: Lens' (Entity vert mat) (Position Float)
entityPosition    = transformation.transPosition

entityScale       :: Lens' (Entity vert mat) (Scale Float)
entityScale       = transformation.transScale

entityOrientation :: Lens' (Entity vert mat) (Orientation Float)
entityOrientation = transformation.transOrientation


instance ( HasResources vert ent ent', HasResources vert env env' ) => 
        HasResources vert (Scene ent env) (Scene ent' env') where
    requestResources scene =
        Scene   <$> ( mapM requestResources $ scene^.sceneEntities )
                <*> ( requestResources $ scene^.sceneEnvironment )
                <*> ( pure $ scene^.sceneCamera )

instance HasResources vert mat mat' => 
        HasResources vert ( Entity (MeshResource vert) mat ) ( Entity (Mesh vert) mat' ) where
    requestResources entity = 
        Entity <$> ( requestMeshResource $ entity^.renderData )
                    <*> ( requestResources $ entity^.materials )
                    <*> ( pure $ entity^.transformation )
                    <*> ( pure $ entity^.drawSettings )


instance ( HasResources vert lit lit', HasResources vert mat mat' ) =>
         HasResources vert ( Environment lit mat) ( Environment lit' mat' ) where
    requestResources env =
        Environment <$> ( mapM requestResources $ env^.envLights )
                    <*> ( mapM loadSky $ env^.envSky )
                    <*> ( pure $ env^.envAmbient )


loadSky :: HasResources vert mat mat' => Sky mat -> YageResources vert (Sky mat')
loadSky sky = do 
    loaded <- requestResources (sky^.materials)
    return $ sky & materials .~ loaded 


toRenderEntity :: ShaderData u t ->
                  Entity (Mesh vert) mat ->
                  RenderEntity vert (ShaderData u t)
toRenderEntity shaderData ent =
    RenderEntity ( ent^.renderData )
                 ( shaderData )
                 ( ent^.drawSettings )

{--
instance (ViableVertex (Vertex geoVert)) => Renderable (Entity geoVert geoMat) geoVert where
    renderDefinition ent =
        let mesh = either (const emptyMesh) id (ent^.renderData)
            texs = either (error "Yage.Scene.Renderable Entity: missing texture: no default texture defined") (id) <$> (ent^.textures)
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
--}

