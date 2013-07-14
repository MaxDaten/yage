{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering.Types where

import qualified   Data.Map                        as Map
import             Foreign.Storable                (sizeOf)

import             Data.Typeable
import             Control.Monad.Reader
import             Control.Monad.State

import             Graphics.GLUtil
import             Graphics.GLUtil.Camera3D
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Core.Raw.FFI
import             Yage.Rendering.WorldState
import             Yage.Resources
-- =================================================================================================


newtype YageRenderer a = YageRenderer (ReaderT YageRenderEnv (StateT RenderState IO) a)
    deriving (Functor, Monad, MonadIO, MonadReader YageRenderEnv, MonadState RenderState, Typeable)

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
    { resources :: YRenderResources 
    }

-- | Loaded resources
data YRenderResources = YRenderResources
    { loadedShaders   :: ![Int] -- dummy
    , linkedPrograms  :: ![ShaderProgram]
    , vaos            :: ![GL.VertexArrayObject]
    }


emptyYRenderResources = YRenderResources [] [] []

initialRenderState = RenderState emptyYRenderResources


---------------------------------------------------------------------------------------------------

class Typeable r => Renderable r where
    render             :: RenderData -> r -> YageRenderer ()

    -- | resources required by the 'Renderable'
    --   this definition will be used to generate the resources for a
    --   'render'-call. If the 'Renderable' leaves the 'RenderScene'
    --   the resources will be freed
    renderModel :: r -> TriMesh
    renderShaders :: r -> [YageShader]


data SomeRenderable = forall r. Renderable r => SomeRenderable r
    deriving (Typeable)


fromRenderable :: Renderable r => SomeRenderable -> Maybe r
fromRenderable (SomeRenderable r) = cast r


instance Renderable SomeRenderable where
    render res (SomeRenderable r) = render res r

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
    --, renderData  :: RenderData
    } deriving (Typeable)

--data ShaderData = ShaderData
--    { program :: ShaderProgram
--    } deriving (Show)

data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: Int
    }

instance Renderable RenderEntity where
    render renderData _{- entity@RenderEntity{..} -} = io $ do
        --GL.linkProgram . program . shaderProgram $ renderData
        withVAO (vao renderData) (drawIndexedTris . fromIntegral . triangleCount $ renderData)


--data Light = Light
--    { lPosition :: Position
--    } deriving (Show)


