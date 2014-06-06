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

import qualified Graphics.Rendering.OpenGL      as GL

data Entity mesh mat = Entity
    { _renderData            :: !mesh
    , _materials             :: !mat
    , _entityTransformation  :: !( Transformation Float )
    , _drawSettings          :: !GLDrawSettings
    }

makeLenses ''Entity

data LightEntity mesh = LightEntity (Entity mesh ()) !Light


data Environment lit sky = Environment
    { _envLights       :: ![ lit ]
    , _envSky          :: !( Maybe sky )
    , _envAmbient      :: !AmbientLight
    }

makeLenses ''Environment



data Scene ent env = Scene
    { _sceneEntities    :: [ ent ]
    , _sceneEnvironment :: env
    , _sceneCamera      :: Camera
    } deriving ( Functor )

makeLenses ''Scene


{--
## Structure access
--}

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


sceneSky :: Lens' (Scene ent (Environment lit sky)) (Maybe sky)
sceneSky = sceneEnvironment.envSky


mkCameraHandle :: V3 Float -> V3 Float -> V3 Float -> Quaternion Float -> V3 Float -> CameraHandle
mkCameraHandle = Cam.Camera

{--
## Entity Shortcuts
--}

entityPosition    :: Lens' (Entity vert mat) (V3 Float)
entityPosition    = entityTransformation.transPosition

entityScale       :: Lens' (Entity vert mat) (V3 Float)
entityScale       = entityTransformation.transScale

entityOrientation :: Lens' (Entity vert mat) (Quaternion Float)
entityOrientation = entityTransformation.transOrientation


instance ( HasResources vert ent ent', HasResources vert env env' ) => 
        HasResources vert (Scene ent env) (Scene ent' env') where
    requestResources scene =
        Scene   <$> ( mapM requestResources $ scene^.sceneEntities )
                <*> ( requestResources $ scene^.sceneEnvironment )
                <*> ( pure $ scene^.sceneCamera )

instance ( HasResources vert mat mat', HasResources vert mesh mesh' ) => 
        HasResources vert ( Entity mesh mat ) ( Entity mesh' mat' ) where
    requestResources entity = 
        Entity <$> ( requestResources $ entity^.renderData )
               <*> ( requestResources $ entity^.materials )
               <*> ( pure $ entity^.entityTransformation )
               <*> ( pure $ entity^.drawSettings )


instance ( HasResources vert lit lit', HasResources vert sky sky' ) =>
         HasResources vert ( Environment lit sky) ( Environment lit' sky' ) where
    requestResources env =
        Environment <$> ( mapM requestResources $ env^.envLights )
                    <*> ( mapM requestResources $ env^.envSky )
                    <*> ( pure $ env^.envAmbient )

instance ( HasResources vert mesh mesh' ) =>
         HasResources vert (LightEntity mesh) (LightEntity mesh') where
         requestResources (LightEntity ent light) =
            LightEntity <$> requestResources ent
                        <*> pure light

{--

loadSky :: HasResources vert mat mat' => Sky MeshResource mat -> YageResources vert (Sky Mesh mat')
loadSky sky = do 
    loaded <- requestResources (sky^.materials)
    return $ sky & materials .~ loaded 
--}


toRenderEntity :: ShaderData u t ->
                  Entity (Mesh vert) mat ->
                  RenderEntity vert (ShaderData u t)
toRenderEntity shaderData ent =
    RenderEntity ( ent^.renderData )
                 ( shaderData )
                 ( ent^.drawSettings )



-- | creates an `Entity` with:
--      an empty `Mesh`
--      the default `Material` as `TexSRGB8`
--      id Transformation
--      settings for triangle primitive rendering and back-face culling
basicEntity :: ( Storable (Vertex geo), Default mat ) => Entity (MeshResource geo) mat
basicEntity =
    Entity 
        { _renderData           = MeshPure emptyMesh
        , _materials            = def
        , _entityTransformation = idTransformation
        , _drawSettings         = GLDrawSettings GL.Triangles (Just GL.Back)
        }

