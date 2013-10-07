{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}
module Yage.Rendering.Types where

import qualified   Data.Map                        as Map
import qualified   Data.Trie                       as T
import             Data.Maybe                      (fromJust)
import             Foreign.Storable                (sizeOf)
import             Foreign.C.Types                 (CDouble(..))

import             Data.Typeable
import             Control.Applicative
import             Control.Monad.RWS.Strict        (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import             Control.Monad.IO.Class
import             Control.Lens                    ((^.))

import             Graphics.GLUtil
import qualified   Graphics.GLUtil                 as GL
import             Graphics.GLUtil.Camera3D        (Camera(..), fpsCamera, camMatrix)
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (($=), Uniform(..), UniformComponent(..))
---------------------------------------------------------------------------------------------------
import             Linear                          ( V3(..), V4(..), M44(..), M33(..), R3(_xyz), Quaternion
                                                   , (!*!)
                                                   , zero, inv33, m33_to_m44, kronecker, fromQuaternion
                                                   , mkTransformation, point, axisAngle, adjoint)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Math
import             Yage.Resources
import             Yage.Rendering.Primitives
import             Yage.Rendering.Shader

import             Debug.Trace
-- =================================================================================================


type Renderer = RWST RenderEnv () RenderState IO
    --deriving (Functor, Applicative, Monad, MonadIO, MonadReader YageRenderEnv, MonadState RenderState, Typeable)

data RenderEnv = RenderEnv
    { envApplication    :: !Application      -- ^ The application to render the frame for
    , envConfig         :: !RenderConfig    -- ^ The current settings for the frame
    }


data RenderConfig = RenderConfig
    { confClearColor        :: !(GL.Color4 Double)
    , confDebugNormals      :: !Bool
    }


data RenderState = RenderState
    { loadedShaders         :: ![(YageShaderResource, ShaderProgram)]
    , loadedMeshes          :: ![(TriMesh, (VBO, EBO))] -- TODO better Key, better structure
    , loadedDefinitions     :: ![(RenderDefinition, VAO)] -- BETTER KEY!!
    , renderStatistics      :: !RenderStatistics
    }

data RenderStatistics = RenderStatistics
    { lastObjectCount       :: !Int
    , lastTriangleCount     :: !Int
    , lastRenderDuration    :: !Double
    , loadedShadersCount    :: !Int
    , loadedMeshesCount     :: !Int
    } deriving Show

initRenderStatistics = RenderStatistics
    { lastObjectCount       = 0
    , lastTriangleCount     = 0
    , lastRenderDuration    = 0.0
    , loadedShadersCount    = 0
    , loadedMeshesCount     = 0
    }

initialRenderState = RenderState 
    { loadedShaders         = []
    , loadedMeshes          = []
    , loadedDefinitions     = []
    , renderStatistics      = initRenderStatistics
    }

type VBO = GL.BufferObject
type EBO = GL.BufferObject


type YageShaderDef = ShaderDefs (GL.VertexArrayDescriptor Int) YageShader
type YageShaderProgram = ShaderProgram
newtype YageShader a = YageShader (Shader YageShaderDef YageShaderProgram Renderer a) -- isolate YageRenderer to one
    deriving (Monad, MonadIO, MonadShader YageShaderDef YageShaderProgram)


shade :: ShaderProgram -> YageShader a -> Renderer a
shade sh (YageShader x) = runShader x globShaderDef sh


globShaderDef :: YageShaderDef
globShaderDef = ShaderDefs
    { sVertexPosition     = mkAttrDef    VertexPos        "in_vert_position" positionVad
    , sVertexNormal       = mkAttrDef    VertexNormal     "in_vert_normal"   normalVad
    , sVertexColor        = mkAttrDef    VertexColor      "in_vert_color"    colorVad
    , sGlobalTime         = mkUniformDef GlobalTime       "global_time"
    , sProjectionMatrix   = mkUniformDef ProjectionMatrix "projection_matrix"
    , sViewMatrix         = mkUniformDef ViewMatrix       "view_matrix"
    , sModelMatrix        = mkUniformDef ModelMatrix      "model_matrix"
    , sNormalMatrix       = mkUniformDef NormalMatrix     "normal_matrix"
    }
    where positionVad   = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 4 GL.Float stride offset0
          normalVad     = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 3 GL.Float stride (offsetPtr $ sizeOf (undefined :: Position))
          colorVad      = let stride = fromIntegral $ sizeOf (undefined::Vertex)
                          in GL.VertexArrayDescriptor 4 GL.Float stride (offsetPtr $ sizeOf (undefined :: Position) + sizeOf (undefined :: Normal))

mkUniformDef :: AsUniform u => (String -> ShaderUniforms String) -> String -> UniformDef u YageShader
mkUniformDef uni s = (uni s, \p v -> io $! v `asUniform` getUniform p s)

mkAttrDef :: vad ~ GL.VertexArrayDescriptor a => (String -> vad -> ShaderAttributes String vad) -> String -> vad -> AttributeDef vad YageShader
mkAttrDef attr s vad = 
    ( attr s vad,
      \p v' -> io $! do
        GL.enableAttrib p s
        GL.setAttrib p s GL.ToFloat vad
    )
---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where
    --render             :: RenderScene -> RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition
    modelAndNormalMatrix :: r -> (M44 GL.GLfloat, M33 GL.GLfloat)
    
    shader :: r -> YageShaderResource
    shader = snd . defs . renderDefinition

    model :: r -> TriMesh
    model = fst . defs . renderDefinition
            


data SomeRenderable = forall r. Renderable r => SomeRenderable r
    deriving (Typeable)


fromRenderable :: Renderable r => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r


instance Renderable SomeRenderable where
    --render scene res (SomeRenderable r) = render scene res r
    renderDefinition (SomeRenderable r) = renderDefinition r
    modelAndNormalMatrix (SomeRenderable r) = modelAndNormalMatrix r


data Renderable r => RenderBatch r = RenderBatch
    { preBatchAction    :: [r] -> Renderer ()
    , perItemAction     :: r -> Renderer ()
    , batch             :: [r]
    }
    

---------------------------------------------------------------------------------------------------


-- how to add statics?!
-- mark ents or seperate?
data RenderScene = RenderScene
    { entities            :: [SomeRenderable]
    , sceneTime           :: !GL.GLfloat
    , viewMatrix          :: !(M44 GL.GLfloat)
    , projectionMatrix    :: !(M44 GL.GLfloat)
    } deriving (Typeable)

camV = V3 0 0 10
emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene [] 0.0 (camMatrix fpsCamera) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)


entitiesCount :: RenderScene -> Int
entitiesCount = length . entities

---------------------------------------------------------------------------------------------------

data RenderEntity = RenderEntity 
    { ePosition    :: !Position
    , eOrientation :: !Orientation
    , eScale       :: !Scale
    , renderDef    :: RenderDefinition
    } deriving (Typeable)

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { ePosition     = zero
    , eOrientation  = axisAngle (V3 0 1 0) (Cam.deg2rad 0)
    , eScale        = (V3 1 1 1)
    , renderDef     = def
    }

data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: !Int
    }

instance Renderable RenderEntity where
    renderDefinition = renderDef
    modelAndNormalMatrix r =
        let scaleM      = mkScale . eScale $ r                                         :: M44 GL.GLfloat
            transformM  = mkTransformation (eOrientation $ r) ((ePosition $ r)^._xyz)  :: M44 GL.GLfloat
            modelM      = transformM !*! scaleM                                        :: M44 GL.GLfloat
            normalM     = adjoint $ fromJust . inv33 . fromTransformation $ modelM               :: M33 GL.GLfloat
        in (modelM, normalM)
        where mkScale = kronecker . point

                

from1 :: UniformComponent a => a -> GL.Index1 a
from1 = GL.Index1
