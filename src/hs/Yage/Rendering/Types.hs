{-# LANGUAGE ExistentialQuantification, RecordWildCards, DeriveFunctor, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering.Types where

import qualified   Data.Map                        as Map
import             Foreign.Storable                (sizeOf)

import             Data.Typeable
import             Control.Monad.Reader

import             Graphics.GLUtil
import             Graphics.GLUtil.Camera3D
import qualified   Graphics.Rendering.OpenGL       as GL
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Core.Raw.FFI
import 			   Yage.Rendering.WorldState
-- =================================================================================================


newtype YageRenderer a = YageRenderer (ReaderT YageRenderEnv IO a)
    deriving (Functor, Monad, MonadIO, MonadReader YageRenderEnv, Typeable)

-- | The context for the 'YageRenderer' reader monad
--   contains all needed data to render a frame
data YageRenderEnv = YageRenderEnv
    { application     :: !YApplication		-- ^ The application to render the frame for
    , window          :: !YGLWindow			-- ^ The window context to render into
    , renderConfig    :: !YRenderConfig 	-- ^ The current settings for the frame 
    , renderResources :: !YRenderResources	-- ^ The needed resources to perform a rendering
    }


data YRenderConfig = YRenderConfig
    { clearColor    :: !(GL.Color4 Double)
    }


-- | Resources for the rendering of a frame
data YRenderResources = YRenderResources
	{ toLoad :: ![IO RenderEntity]			-- ^ loading actions which results in a RenderEntity
	, active :: ![RenderEntity]				-- ^ current loaded an ready to be rendered entities
	, toFree :: ![RenderEntity]				-- ^ ready to be freed in context
	}

emptyYRenderResources = YRenderResources [] [] []


---------------------------------------------------------------------------------------------------


class Renderable r where
    render :: r -> YageRenderer ()


data SomeRenderable = forall r. Renderable r => SomeRenderable r

instance Renderable SomeRenderable where
	render (SomeRenderable r) = render r

---------------------------------------------------------------------------------------------------

data RenderScene = RenderScene
    { -- view :: RenderView
     entities :: [SomeRenderable]
    --, lights   :: [Light]
    }

instance Renderable RenderScene where
    render w = mapM_ render (entities w)


emptyRenderScene :: RenderScene
emptyRenderScene = RenderScene []


---------------------------------------------------------------------------------------------------

data RenderEntity = RenderEntity 
    { ePosition   :: Position -- orientation and so on
    , renderData  :: RenderData
    }

--data ShaderData = ShaderData
--    { program :: ShaderProgram
--    } deriving (Show)

data RenderData = RenderData
    { vao           :: GL.VertexArrayObject
    , shaderProgram :: ShaderProgram
    , triangleCount :: Int
    }

instance Renderable RenderEntity where
    render entity@RenderEntity{..} = io $ do
        --GL.linkProgram . program . shaderProgram $ renderData
        withVAO (vao renderData) (drawIndexedTris . fromIntegral . triangleCount $ renderData)


--data Light = Light
--    { lPosition :: Position
--    } deriving (Show)


type Position = V3 Double
type Orientation = Quaternion Double

type Vertex = V3 Float
type Index = Int

data TriMesh = TriMesh
    { vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    }

mkTriMesh :: [Vertex] -> [Index] -> TriMesh
-- some assertions for invalid meshes
mkTriMesh vs ixs = TriMesh vs ixs ((length ixs) `quot` 3)

positionAttrib = "position"
