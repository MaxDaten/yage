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


import           Yage.Prelude                   hiding ( mapM )
import           Yage.Lens

import           Yage.Camera
import           Yage.Light
import           Yage.Resources                 as Res
import           Yage.Geometry                  hiding ( Face )

import qualified Data.Sequence                  as S

import qualified Graphics.GLUtil.Camera3D       as Cam

import           Yage.Rendering.RenderEntity
import           Yage.Rendering
import           Yage.Transformation
import qualified Graphics.Rendering.OpenGL      as GL

data Entity mesh mat = Entity
    { _renderData            :: !mesh
    , _materials             :: !mat
    , _entityTransformation  :: !( Transformation Double )
    , _drawSettings          :: !GLDrawSettings
    }

makeLenses ''Entity

data LightEntity mesh = LightEntity (Entity mesh ()) !Light


data Environment lit sky = Environment
    { _envLights       :: Seq lit
    , _envSky          :: ( Maybe sky )
    , _envAmbient      :: AmbientLight
    }

makeLenses ''Environment



data Scene cam ent env gui = Scene
    { _sceneEntities    :: Seq ent
    , _sceneEnvironment :: env
    , _sceneCamera      :: cam
    , _sceneGui         :: gui
    } deriving ( Show )

makeLenses ''Scene


{--
## Structure access
--}

emptyScene :: cam -> gui -> Scene cam ent (Environment lit skymat) gui
emptyScene cam = Scene S.empty emptyEnvironment cam
{-# INLINE emptyScene #-}


emptyEnvironment :: Environment lit mat
emptyEnvironment = Environment S.empty Nothing (AmbientLight 0)
{-# INLINE emptyEnvironment #-}


addEntity :: Scene cam ent env dat -> ent -> Scene cam ent env dat
addEntity scene ent = scene & sceneEntities %~ (S.|> ent)
{-# INLINE addEntity #-}


addLight :: Scene cam ent (Environment lit mat) dat -> lit -> Scene cam ent (Environment lit mat) dat
addLight scene l = scene & sceneLights %~ (S.|> l)
{-# INLINE addLight #-}


entitiesCount :: Scene cam ent (Environment l m) dat -> Int
entitiesCount = length . _sceneEntities
{-# INLINE entitiesCount #-}


lightCount :: Scene cam ent (Environment l m) dat -> Int
lightCount = length . _envLights . _sceneEnvironment
{-# INLINE lightCount #-}


sceneSky :: Lens' (Scene cam ent (Environment lit sky) dat) (Maybe sky)
sceneSky = sceneEnvironment.envSky
{-# INLINE sceneSky #-}

sceneLights :: Lens' (Scene cam ent (Environment lit sky) dat) (Seq lit)
sceneLights = sceneEnvironment.envLights
{-# INLINE sceneLights #-}


mkCameraHandle :: V3 Double -> V3 Double -> V3 Double -> Quaternion Double -> V3 Double -> CameraHandle
mkCameraHandle = Cam.Camera

{--
## Entity Shortcuts
--}

entityPosition    :: Lens' (Entity vert mat) (V3 Double)
entityPosition    = entityTransformation.transPosition


entityScale       :: Lens' (Entity vert mat) (V3 Double)
entityScale       = entityTransformation.transScale


entityOrientation :: Lens' (Entity vert mat) (Quaternion Double)
entityOrientation = entityTransformation.transOrientation


lightEntity :: Lens' (LightEntity mesh) (Entity mesh ())
lightEntity = lens getter setter where
  getter (LightEntity entity _) = entity
  setter (LightEntity _ light) entity = LightEntity entity light


entityLight :: Lens' (LightEntity mesh) Light
entityLight = lens getter setter where
  getter (LightEntity _ light) = light
  setter (LightEntity entity _) light = LightEntity entity light

{--
instance ( HasResources vert ent ent', HasResources vert env env'
         , HasResources vert gui gui'
         ) =>
        HasResources vert (Scene cam ent env gui) (Scene cam ent' env' gui') where
    requestResources scene =
        Scene   <$> ( mapM requestResources $ scene^.sceneEntities )
                <*> ( requestResources $ scene^.sceneEnvironment )
                <*> ( pure $ scene^.sceneCamera )
                <*> ( requestResources $ scene^.sceneGui)


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
--}

toRenderEntity :: ShaderData u t ->
                  Entity (Mesh vert) mat ->
                  RenderEntity vert (ShaderData u t)
toRenderEntity shaderData ent =
    RenderEntity ( ent^.renderData )
                 ( shaderData )
                 ( ent^.drawSettings )



-- | creates an `Entity` with:
--     - an empty `Mesh`
--     - the default `Material` as `TexSRGB8`
--     - id Transformation
--     - settings for triangle primitive rendering and back-face culling
basicEntity :: ( Storable (Vertex vert), Default mat ) => Entity (Mesh (Vertex vert)) mat
basicEntity =
    Entity
        { _renderData           = emptyMesh
        , _materials            = def
        , _entityTransformation = idTransformation
        , _drawSettings         = GLDrawSettings GL.Triangles (Just GL.Back)
        }


-- TODO: Material Interpolation?
instance LinearInterpolatable (Entity a b) where
    lerp alpha u v = u & entityTransformation .~ lerp alpha (u^.entityTransformation) (v^.entityTransformation)


instance ( LinearInterpolatable cam
         , LinearInterpolatable ent
         , LinearInterpolatable env
         , LinearInterpolatable dat
         ) => LinearInterpolatable (Scene cam ent env dat) where
    lerp alpha u v =
        u & sceneEntities    .~ zipWith (lerp alpha) (u^.sceneEntities) (v^.sceneEntities)
          & sceneEnvironment .~ lerp alpha (u^.sceneEnvironment) (v^.sceneEnvironment)
          & sceneCamera      .~ lerp alpha (u^.sceneCamera) (v^.sceneCamera)

instance (LinearInterpolatable lit, LinearInterpolatable sky) => LinearInterpolatable (Environment lit sky) where
    lerp alpha u v =
        u & envLights  .~ zipWith (lerp alpha) (u^.envLights) (v^.envLights)
          & envSky     .~ (lerp alpha <$> u^.envSky <*> v^.envSky )
          & envAmbient .~ (lerp alpha (u^.envAmbient) (v^.envAmbient))

instance LinearInterpolatable (LightEntity mesh) where
    lerp alpha (LightEntity eu lu) (LightEntity ev lv) = LightEntity (lerp alpha eu ev) (lerp alpha lu lv)

