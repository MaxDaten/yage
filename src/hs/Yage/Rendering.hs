{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering (
      module GLReExports
    , runYageRenderer
    , renderScene

    ) where

import Debug.Trace

import qualified   Data.Map                        as Map
import             Data.List                       (groupBy)
import             Foreign.Storable                (sizeOf)
import             Control.Concurrent              (threadDelay)
import             System.Mem                      (performGC)

import             Data.Typeable
import             Control.Applicative
import             Control.Monad.Reader
import             Control.Monad.State

import             Graphics.GLUtil                 hiding (makeVAO)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), zero)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import             Yage.Core.Raw.FFI
import 			   Yage.Rendering.Types
import             Yage.Rendering.WorldState
import             Yage.Rendering.Shader           ((.=))
import qualified   Yage.Rendering.Shader           as Shader
import             Yage.Rendering.Utils
import 			   Yage.Resources
{-=================================================================================================-}


renderScene :: RenderScene -> YageRenderer ()
renderScene scene = renderFrame scene >> afterFrame



afterFrame :: YageRenderer ()
afterFrame = io $ do
    -- this should not be part of the rendering, indeed
    performGC
    --threadDelay (16*1000) -- 60fps
    return ()


renderFrame :: RenderScene -> YageRenderer ()
renderFrame scene = do
    beforeRender
    doRender scene
    afterRender


doRender :: RenderScene -> YageRenderer ()
doRender scene@RenderScene{..} = do
    let batches = createShaderBatches scene entities
    mapM_ renderBatch batches --- insert batch rendering here


renderWithData :: RenderScene -> SomeRenderable -> YageRenderer ()
renderWithData scene r = requestRenderData r >>= \res -> render scene res r


renderBatch :: RenderBatch SomeRenderable -> YageRenderer ()
renderBatch b@RenderBatch{..} = preBatch >> mapM_ perItem batch


createShaderBatches :: RenderScene -> [SomeRenderable] -> [RenderBatch SomeRenderable]
createShaderBatches scene rs = 
    let shaderGroups = groupBy sameShader rs
    in map mkShaderBatch shaderGroups
    where
        sameShader :: SomeRenderable -> SomeRenderable -> Bool
        sameShader a b = shader a == shader b
        mkShaderBatch :: [SomeRenderable] -> RenderBatch SomeRenderable
        mkShaderBatch rs =
            let batchShader = shader . head $ rs
            in RenderBatch
                { preBatch = do
                    shader <- requestShader batchShader 
                    io $ GL.currentProgram $= Just (program shader)
                    setSceneGlobals scene shader
                , perItem = renderWithData scene
                , batch = rs
                }



beforeRender :: YageRenderer ()
beforeRender = do
    setupFrame
    prepareResources


setupFrame :: YageRenderer ()
setupFrame = withWindow $ \win -> do
    clearC <- asks $ clearColor . renderConfig
    io $ do
        beginDraw $ win

        GL.clearColor $= fmap realToFrac clearC
        GL.depthFunc $= Just GL.Less -- to init
        GL.depthMask $= GL.Enabled      -- to init
        
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]


        w <- width win
        h <- height win
        r <- return . floor =<< pixelRatio win
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


-- | Unloads unneeded render-resources and loads needed resources
prepareResources :: YageRenderer ()
prepareResources = return ()

---------------------------------------------------------------------------------------------------


afterRender :: YageRenderer ()
afterRender = withWindow $ \win -> io . endDraw $ win

---------------------------------------------------------------------------------------------------

render :: RenderScene -> RenderData -> SomeRenderable -> YageRenderer ()
render scene rd@RenderData{..} r = do
    shadeItem shaderProgram scene r
    io $ withVAO vao $ drawIndexedTris . fromIntegral $ triangleCount


setSceneGlobals :: RenderScene -> YageShaderProgram -> YageRenderer ()
setSceneGlobals scene sProg = shade sProg $ do
    Shader.sGlobalTime       .= sceneTime scene
    Shader.sProjectionMatrix .= projectionMatrix scene
    Shader.sViewMatrix       .= viewMatrix scene


shadeItem :: YageShaderProgram -> RenderScene -> SomeRenderable -> YageRenderer ()
shadeItem sProg scene r = shade sProg $ do
    Shader.sModelMatrix      .= (modelMatrix $! r)
---------------------------------------------------------------------------------------------------


withWindow :: (YGLWindow -> YageRenderer a) -> YageRenderer a
withWindow f = asks window >>= f


withApplication :: (YApplication -> YageRenderer a) -> YageRenderer a
withApplication f = asks application >>= f


---------------------------------------------------------------------------------------------------


-- | runs the renderer in the given environment to render one frame.
-- TODO :: combine this with the scene setup
runYageRenderer :: YageRenderer a -> RenderState -> YageRenderEnv -> IO (a, RenderState)
runYageRenderer (YageRenderer a) state env = runStateT (runReaderT a env) state

---------------------------------------------------------------------------------------------------

requestRenderData :: SomeRenderable -> YageRenderer RenderData
requestRenderData r = do
    sh  <- requestShader $ shader r
    vao <- requestVAO $ renderDefinition r
    return $ RenderData vao sh (triCount . model $ r)

requestRenderResource :: Eq a 
                  => (RenderState -> [(a, b)])                  -- ^ accassor function for state
                  -> (a -> YageRenderer b)                      -- ^ load function for resource
                  -> ((a,b) -> YageRenderer ())                 -- ^ function to add loaded resource to state
                  -> a                                          -- ^ the value to load resource from
                  -> YageRenderer b                             -- ^ the loaded resource
requestRenderResource accessor loadResource addResource a = do
    rs <- gets accessor
    maybe (loadResource a >>= \r -> addResource (a, r) >> (return $! r))
        return
        (lookup a rs)

requestVAO :: RenderDefinition -> YageRenderer (VAO)
requestVAO = requestRenderResource loadedDefinitions loadDefinition addDefinition
    where
        loadDefinition :: RenderDefinition -> YageRenderer (VAO)
        loadDefinition (RenderDefinition (mesh, shader)) = do
            (vbo, ebo) <- requestMesh mesh
            sProg      <- requestShader shader

            makeVAO $ do
                io $ GL.bindBuffer GL.ArrayBuffer $= Just vbo
                io $ GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
                shade sProg $ Shader.enableAttrib Shader.sVertexPosition

        addDefinition :: (RenderDefinition, VAO) -> YageRenderer ()
        addDefinition d = modify $ \st -> st{ loadedDefinitions = d:(loadedDefinitions st) }


requestShader :: YageShaderResource -> YageRenderer (YageShaderProgram)
requestShader = requestRenderResource loadedShaders loadShaders addShader
    where
        loadShaders :: YageShaderResource -> YageRenderer (YageShaderProgram)
        loadShaders shader = do
            sProg <- io $! loadShaderProgram (vert shader) (frag shader)
            return sProg

        addShader :: (YageShaderResource, YageShaderProgram) -> YageRenderer ()
        addShader s = modify $! \st -> st{ loadedShaders = s:(loadedShaders st) }



requestMesh :: TriMesh -> YageRenderer (VBO, EBO)
requestMesh = requestRenderResource loadedMeshes loadMesh addMesh
    where
        loadMesh :: TriMesh -> YageRenderer (VBO, EBO)
        loadMesh mesh = io $ do
            vbo <- makeBuffer GL.ArrayBuffer $ vertices mesh
            ebo <- bufferIndices $ map fromIntegral $ indices mesh
            print "mesh loaded"
            return (vbo, ebo)

        addMesh :: (TriMesh, (VBO, EBO)) -> YageRenderer ()
        addMesh m = modify $! \st -> st{ loadedMeshes = m:(loadedMeshes st) }

