{-# LANGUAGE RecordWildCards, ExistentialQuantification, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering where


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
import 			   Yage.Rendering.Scene
---------------------------------------------------------------------------------------------------


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

runYageRenderer :: YageRenderer a -> YageRenderEnv -> IO (a)
runYageRenderer (YageRenderer a) env = runReaderT a env

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

---------------------------------------------------------------------------------------------------

mkRenderEntity :: TriMesh -> ShaderProgram -> IO RenderEntity
mkRenderEntity TriMesh{..} sprog = do
    vao <- io $ makeVAO $ do
        vbo <- makeBuffer GL.ArrayBuffer vertices
        GL.bindBuffer GL.ArrayBuffer $= Just vbo

        --enableAttrib sprog positionAttrib
        --let stride = fromIntegral $ sizeOf (undefined::Vertex) * 3
        --    vad = GL.VertexArrayDescriptor 3 GL.Float stride offset0
        --setAttrib sprog positionAttrib GL.ToFloat vad
        
        ebo <- bufferIndices $ map fromIntegral indices
        GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
        print $ "renderable " ++ show vbo
    return $ RenderEntity zero (RenderData vao sprog triCount)

---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------

drawScene :: RenderScene -> YageRenderer ()
drawScene scene = (renderFrame scene) >> afterFrame



afterFrame :: YageRenderer ()
afterFrame = return ()


renderFrame :: RenderScene -> YageRenderer ()
renderFrame scene = do
    beforeRender
    doRender scene
    afterRender


doRender :: RenderScene -> YageRenderer ()
doRender = render


beforeRender :: YageRenderer ()
beforeRender = do
    clearC <- asks $ clearColor . renderConfig
    withWindow $ io . \win -> do
        beginDraw $ win

        GL.clearColor $= fmap realToFrac clearC
        GL.clear [GL.ColorBuffer]

        w <- width win
        h <- height win
        r <- return . floor =<< pixelRatio win
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


afterRender :: YageRenderer ()
afterRender = withWindow $ \win -> io . endDraw $ win

---------------------------------------------------------------------------------------------------


withWindow :: (YGLWindow -> YageRenderer a) -> YageRenderer a
withWindow f = asks window >>= f


withApplication :: (YApplication -> YageRenderer a) -> YageRenderer a
withApplication f = asks application >>= f


---------------------------------------------------------------------------------------------------

extractRenderScene :: Scene -> RenderScene
extractRenderScene scene = RenderScene [] -- TODO

--addRenderable :: SomeRenderable -> Yage ()
--addRenderable r = modify $ \s@YageState{..} -> s{ resources = YageResources (r : (renderables resources)) }

