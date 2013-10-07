{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Yage.Rendering (
      module GLReExports
    , runRenderer
    , renderScene

    ) where

import Debug.Trace

import qualified   Data.Map                        as Map
import qualified   Data.Trie                       as T
import             Data.List                       (groupBy)

import             Data.ByteString.Char8           (ByteString)


import             Foreign.Storable                (sizeOf)
import             Control.Concurrent              (threadDelay)
import             System.Mem                      (performGC)

import             Data.Typeable
import             Control.Applicative

import             Control.Monad.RWS.Strict        (ask, asks, runRWST, get, gets, liftIO, modify, put)


import             Graphics.GLUtil                 hiding (makeVAO)
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import             Graphics.Rendering.OpenGL.GL    as GLReExports (Color4(..))
---------------------------------------------------------------------------------------------------
import             Linear                          (V3(..), R3(_xyz), zero)
import             Linear.Quaternion               (Quaternion)
---------------------------------------------------------------------------------------------------
import             Yage.Import
import 			   Yage.Rendering.Types
import             Yage.Rendering.WorldState
import             Yage.Rendering.Shader           ((.=))
import qualified   Yage.Rendering.Shader           as Shader
import             Yage.Rendering.Utils
import 			   Yage.Resources
{-=================================================================================================-}


renderScene :: RenderScene -> Renderer ()
renderScene scene = renderFrame scene >> afterFrame



afterFrame :: Renderer ()
afterFrame = io $ do
    -- this should not be part of the rendering, indeed
    performGC
    --threadDelay (16*1000) -- 60fps
    return ()


renderFrame :: RenderScene -> Renderer ()
renderFrame scene = do
    beforeRender
    
    (objCount, renderTime) <- ioTime $ doRender scene

    shCount <- gets $! length . loadedShaders
    mshCount <- gets $! length . loadedMeshes
    let stats = RenderStatistics
            { lastObjectCount    = objCount
            , lastRenderDuration = renderTime
            , lastTriangleCount  = sum $! map (triCount . model) $ entities scene
            , loadedShadersCount = shCount
            , loadedMeshesCount  = mshCount
            }

    afterRender stats


doRender :: RenderScene -> Renderer Int
doRender scene@RenderScene{..} =
    let batches = createShaderBatches scene entities
    in sum `liftM` mapM renderBatch batches


renderWithData :: RenderScene -> SomeRenderable -> Renderer ()
renderWithData scene r = requestRenderData r >>= \res -> render scene res r


renderBatch :: RenderBatch SomeRenderable -> Renderer Int
renderBatch b@RenderBatch{..} = preBatchAction batch >> length `liftM` mapM perItemAction batch


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
                { preBatchAction = \_ -> do
                    shader <- requestShader batchShader 
                    io $! GL.currentProgram $= Just (program shader)
                    setSceneGlobals scene shader
                , perItemAction = renderWithData scene
                , batch = rs
                }



beforeRender :: Renderer ()
beforeRender = do
    setupFrame
    prepareResources


setupFrame :: Renderer ()
setupFrame = withWindow $ \win -> do
    clearC <- asks $ confClearColor . envConfig
    io $! do
        beginDraw $ win

        GL.clearColor $= fmap realToFrac clearC
        GL.depthFunc $= Just GL.Less -- to init
        GL.depthMask $= GL.Enabled      -- to init
        
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]


        w <- winWidth win
        h <- winHeight win
        r <- return . floor =<< pixelRatio win
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


-- | Unloads unneeded render-resources and loads needed resources
prepareResources :: Renderer ()
prepareResources = return ()

---------------------------------------------------------------------------------------------------


afterRender :: RenderStatistics -> Renderer ()
afterRender stats = do
    withWindow $ \win -> io . endDraw $ win
    updateStatistics stats
            

---------------------------------------------------------------------------------------------------

render :: RenderScene -> RenderData -> SomeRenderable -> Renderer ()
render scene rd@RenderData{..} r = do
    shadeItem shaderProgram scene r
    io $! withVAO vao $! drawIndexedTris . fromIntegral $ triangleCount


setSceneGlobals :: RenderScene -> YageShaderProgram -> Renderer ()
setSceneGlobals scene sProg = shade sProg $! do
    Shader.sGlobalTime       .= sceneTime scene
    Shader.sProjectionMatrix .= projectionMatrix scene
    Shader.sViewMatrix       .= viewMatrix scene


shadeItem :: YageShaderProgram -> RenderScene -> SomeRenderable -> Renderer ()
shadeItem sProg scene r = shade sProg $! do
    let (modelM, normalM) = modelAndNormalMatrix $! r
    Shader.sModelMatrix      .= modelM
    Shader.sNormalMatrix     .= normalM
---------------------------------------------------------------------------------------------------


withWindow :: ByteString -> (Maybe Window -> Renderer a) -> Renderer a
withWindow name f = asks (T.lookup name . appWindows) >>= f


withApplication :: (Application -> Renderer a) -> Renderer a
withApplication f = asks envApplication >>= f


---------------------------------------------------------------------------------------------------


-- | runs the renderer in the given environment to render one frame.
-- TODO :: combine this with the scene setup
runRenderer :: Renderer a -> RenderState -> RenderEnv -> IO (a, RenderState)
runRenderer renderer state env = runRWST env state

---------------------------------------------------------------------------------------------------

requestRenderData :: SomeRenderable -> Renderer RenderData
requestRenderData r = do
    sh  <- requestShader $ shader r
    vao <- requestVAO $ renderDefinition r
    return $ RenderData vao sh (triCount . model $ r)

requestRenderResource :: Eq a 
                  => (RenderState -> [(a, b)])                  -- ^ accassor function for state
                  -> (a -> Renderer b)                      -- ^ load function for resource
                  -> ((a,b) -> Renderer ())                 -- ^ function to add loaded resource to state
                  -> a                                          -- ^ the value to load resource from
                  -> Renderer b                             -- ^ the loaded resource
requestRenderResource accessor loadResource addResource a = do
    rs <- gets accessor
    maybe (loadResource a >>= \r -> addResource (a, r) >> (return $! r))
        return
        (lookup a rs)

requestVAO :: RenderDefinition -> Renderer (VAO)
requestVAO = requestRenderResource loadedDefinitions loadDefinition addDefinition
    where
        loadDefinition :: RenderDefinition -> Renderer (VAO)
        loadDefinition (RenderDefinition (mesh, shader)) = do
            (vbo, ebo) <- requestMesh mesh
            sProg      <- requestShader shader

            makeVAO $ do
                io $ GL.bindBuffer GL.ArrayBuffer $= Just vbo
                io $ GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
                shade sProg $ do
                    Shader.enableAttrib Shader.sVertexPosition
                    Shader.enableAttrib Shader.sVertexNormal
                    Shader.enableAttrib Shader.sVertexColor


requestShader :: YageShaderResource -> Renderer (YageShaderProgram)
requestShader = requestRenderResource loadedShaders loadShaders addShader
    where
        loadShaders :: YageShaderResource -> Renderer (YageShaderProgram)
        loadShaders shader = do
            sProg <- io $! loadShaderProgram (vert shader) (frag shader)
            return $! sProg


requestMesh :: TriMesh -> Renderer (VBO, EBO)
requestMesh = requestRenderResource loadedMeshes loadMesh addMesh
    where
        loadMesh :: TriMesh -> Renderer (VBO, EBO)
        loadMesh mesh 
            | traceShow "start loading mesh" False = undefined
            | otherwise = io $ do
                print $ "mesh loaded1" ++ show mesh
                vbo <- makeBuffer GL.ArrayBuffer $ vertices $ mesh
                print "mesh loaded2"
                ebo <- bufferIndices $ map fromIntegral $ indices mesh
                print "mesh loaded3"
                return $! (vbo, ebo)

---------------------------------------------------------------------------------------------------

addMesh :: (TriMesh, (VBO, EBO)) -> Renderer ()
addMesh m = modify $! \st -> st{ loadedMeshes = m:(loadedMeshes st) }


addShader :: (YageShaderResource, YageShaderProgram) -> Renderer ()
addShader s = modify $! \st -> st{ loadedShaders = s:(loadedShaders st) }


addDefinition :: (RenderDefinition, VAO) -> Renderer ()
addDefinition d = modify $ \st -> st{ loadedDefinitions = d:(loadedDefinitions st) }


updateStatistics :: RenderStatistics -> Renderer ()
updateStatistics stats = modify $ \st -> st{ renderStatistics = stats }

