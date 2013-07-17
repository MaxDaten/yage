{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering.Types where

import qualified   Data.Map                        as Map
import             Foreign.Storable                (sizeOf)
import             Foreign.C.Types                 (CDouble(..))

import             Data.Typeable
import             Control.Applicative
import             Control.Monad.Reader
import             Control.Monad.State

import             Graphics.GLUtil
import             Graphics.GLUtil.Camera3D        (Camera(..), fpsCamera, camMatrix)
import qualified   Graphics.GLUtil.Camera3D        as Cam
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (($=), Uniform(..), UniformComponent(..))
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), M44(..), (!*!), zero, m33_to_m44, kronecker, mkTransformation, axisAngle)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Core.Raw.FFI
--import             Yage.Rendering.WorldState
import             Yage.Resources
import             Yage.Rendering.Primitives
-- =================================================================================================


newtype YageRenderer a = YageRenderer (ReaderT YageRenderEnv (StateT RenderState IO) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader YageRenderEnv, MonadState RenderState, Typeable)

-- | The context for the 'YageRenderer' reader monad
--   contains all needed data to render a frame
data YageRenderEnv = YageRenderEnv
    { application     :: !YApplication      -- ^ The application to render the frame for
    , window          :: !YGLWindow         -- ^ The window context to render into
    , renderConfig    :: !YRenderConfig     -- ^ The current settings for the frame 
    }


data YRenderConfig = YRenderConfig
    { clearColor    :: !(GL.Color4 Double)
    }


data RenderState = RenderState
    { loadedShaders       :: ![(YageShader, ShaderProgram)]
    , loadedMeshes        :: ![(TriMesh, (VBO, EBO))] -- TODO better Key
    , loadedDefinitions   :: ![(RenderDefinition, VAO)] -- BETTER KEY!!
    }

initialRenderState = RenderState [] [] []

type VBO = GL.BufferObject
type EBO = GL.BufferObject

---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where
    --render             :: RenderScene -> RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition
    modelMatrix :: r -> M44 GL.GLfloat
    
    shader :: r -> YageShader
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
    modelMatrix (SomeRenderable r) = modelMatrix r

---------------------------------------------------------------------------------------------------

data RenderScene = RenderScene
    { entities            :: [SomeRenderable]
    , sceneTime           :: GL.GLfloat
    , viewMatrix          :: M44 GL.GLfloat
    , projectionMatrix    :: M44 GL.GLfloat
    } deriving (Typeable)

--instance Renderable RenderScene where
--    render res scene = mapM_ (render res) (entities scene)

camV = V3 0 0 10
emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene [] 0.0 (camMatrix fpsCamera) (Cam.projectionMatrix (Cam.deg2rad 60) 1 1 45)


---------------------------------------------------------------------------------------------------

data RenderEntity = RenderEntity 
    { ePosition    :: Position
    , eOrientation :: Orientation
    , eScale       :: Scale
    , renderDef    :: RenderDefinition
    } deriving (Typeable)

mkRenderEntity :: RenderDefinition -> RenderEntity
mkRenderEntity def = RenderEntity
    { ePosition = zero
    , eOrientation = axisAngle (V3 0 1 0) (Cam.deg2rad 0)
    , eScale = V3 1 1 1
    , renderDef = def
    }

data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: Int
    }

instance Renderable RenderEntity where
    renderDefinition = renderDef
    modelMatrix r = let  scaleM = m33_to_m44 . kronecker $ eScale r
                         transformM = mkTransformation (eOrientation r) (ePosition r)
                    in transformM !*! scaleM

from1 :: UniformComponent a => a -> GL.Index1 a
from1 = GL.Index1
--instance UniformComponent a => Uniform a where
--    -- uniform :: UniformLocation -> StateVar a
--    uniform = undefined
--    -- uniformv :: UniformLocation -> GLsizei -> Ptr a -> IO ()
--    uniformv = undefined

