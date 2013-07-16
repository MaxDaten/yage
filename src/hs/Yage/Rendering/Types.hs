{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering.Types where

import qualified   Data.Map                        as Map
import             Foreign.Storable                (sizeOf)

import             Data.Typeable
import             Control.Applicative
import             Control.Monad.Reader
import             Control.Monad.State

import             Graphics.GLUtil
import             Graphics.GLUtil.Camera3D
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL       (($=))
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Core.Raw.FFI
import             Yage.Rendering.WorldState
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
    render             :: RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderDefinition :: r -> RenderDefinition
    
    model :: r -> TriMesh
    model = fst . defs . renderDefinition

    shader :: r -> YageShader
    shader = snd . defs . renderDefinition


data SomeRenderable = forall r. Renderable r => SomeRenderable r
    deriving (Typeable)


fromRenderable :: Renderable r => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r


instance Renderable SomeRenderable where
    render res (SomeRenderable r) = render res r
    renderDefinition (SomeRenderable r) = renderDefinition r

---------------------------------------------------------------------------------------------------

data RenderScene = RenderScene
    { entities :: [SomeRenderable]
    } deriving (Typeable)

--instance Renderable RenderScene where
--    render res scene = mapM_ (render res) (entities scene)


emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene []


---------------------------------------------------------------------------------------------------

data RenderEntity = RenderEntity 
    { ePosition   :: Position -- and orientation and so on
    , renderDef   :: RenderDefinition
    } deriving (Typeable)


data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: Int
    }

instance Renderable RenderEntity where
    render renderData _ = io $ do
        GL.currentProgram $= Just (program . shaderProgram $ renderData)
        withVAO (vao renderData) (drawIndexedTris . fromIntegral . triangleCount $ renderData)
    
    renderDefinition = renderDef



