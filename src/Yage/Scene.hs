{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
module Yage.Scene
    ( module Yage.Scene
    , module Yage.Light
    , module Res
    ) where


import           Yage.Prelude                   hiding ( mapM )
import           Yage.Lens
import           Yage.Camera

import           Yage.Light
import           Yage.Resources                 as Res

import qualified Data.Sequence                  as S
import           Data.Data

import           Yage.Transformation

data Entity mesh mat = Entity
  { _renderData            :: !mesh
  , _materials             :: !mat
  , _entityTransformation  :: !( Transformation Double )
  -- , _drawSettings          :: !GLDrawSettings
  }

makeLenses ''Entity

data LightEntity mesh = LightEntity mesh !Light

data Lights l = Lights
  { _lightsPoint :: l
  , _lightsSpot  :: l
  , _lightsDir   :: l
  } deriving (Show,Ord,Eq,Functor,Traversable,Foldable,Typeable,Data,Generic)

makeFields ''Lights

data Environment lit sky = Environment
  { _environmentLights       :: Lights (Seq lit)
  , _environmentSky          :: ( Maybe sky )
  , _environmentAmbient      :: AmbientLight
  } deriving (Show,Ord,Eq,Typeable,Data,Generic)

makeFields ''Environment


data Scene ent env = Scene
  { _sceneEntities    :: Seq ent
  , _sceneEnvironment :: env
  } deriving ( Show )

makeFields ''Scene
makeClassy ''Scene

emptyEnvironment :: Environment lit mat
emptyEnvironment = Environment (Lights S.empty S.empty S.empty) Nothing (AmbientLight 0)
{-# INLINE emptyEnvironment #-}

emptyScene :: Scene ent (Environment lit sky)
emptyScene = Scene S.empty emptyEnvironment
{-# INLINE emptyScene #-}


{--

-- * Structure access

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


lightEntity :: Lens' (LightEntity mesh) (Entity mesh ())
lightEntity = lens getter setter where
  getter (LightEntity entity _) = entity
  setter (LightEntity _ light) entity = LightEntity entity light


entityLight :: Lens' (LightEntity mesh) Light
entityLight = lens getter setter where
  getter (LightEntity _ light) = light
  setter (LightEntity entity _) light = LightEntity entity light


-- toRenderEntity :: ShaderData u t ->
--                   Entity (Mesh vert) mat ->
--                   RenderEntity vert (ShaderData u t)
-- toRenderEntity shaderData ent =
--     RenderEntity ( ent^.renderData )
--                  ( shaderData )
--                  ( ent^.drawSettings )

--}


-- | creates an `Entity` with:
--     - an empty `Mesh`
--     - the default `Material` as `TexSRGB8`
--     - id Transformation
--     - settings for triangle primitive rendering and back-face culling
basicEntity :: ( Storable v, Default mat ) => Entity (Mesh v) mat
basicEntity =
    Entity
        { _renderData           = emptyMesh
        , _materials            = def
        , _entityTransformation = idTransformation
        -- , _drawSettings         = GLDrawSettings GL.Triangles (Just GL.Back)
        }

instance HasTransformation (Entity e g) Double where
  transformation = entityTransformation

-- TODO: Material Interpolation?
instance LinearInterpolatable (Entity a b) where
    lerp alpha u v = u & entityTransformation .~ lerp alpha (u^.entityTransformation) (v^.entityTransformation)


-- instance ( LinearInterpolatable cam
--          , LinearInterpolatable ent
--          , LinearInterpolatable env
--          , LinearInterpolatable dat
--          ) => LinearInterpolatable (Scene cam ent env dat) where
--     lerp alpha u v =
--         u & sceneEntities    .~ zipWith (lerp alpha) (u^.sceneEntities) (v^.sceneEntities)
--           & sceneEnvironment .~ lerp alpha (u^.sceneEnvironment) (v^.sceneEnvironment)
--           & sceneCamera      .~ lerp alpha (u^.sceneCamera) (v^.sceneCamera)

-- instance (LinearInterpolatable lit, LinearInterpolatable sky) => LinearInterpolatable (Environment lit sky) where
--     lerp alpha u v =
--         u & ights  .~ zipWith (lerp alpha) (u^.envLights) (v^.envLights)
--           & envSky     .~ (lerp alpha <$> u^.envSky <*> v^.envSky )
--           & envAmbient .~ (lerp alpha (u^.envAmbient) (v^.envAmbient))

instance LinearInterpolatable (LightEntity mesh) where
    lerp alpha (LightEntity _ lu) (LightEntity mesh lv) = LightEntity mesh (lerp alpha lu lv)

