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
import 			   Yage.Rendering.Types
import 			   Yage.Rendering.WorldState
-- =================================================================================================

drawScene :: RenderScene -> YageRenderer ()
drawScene scene = renderFrame scene >> afterFrame



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

runYageRenderer :: YageRenderer a -> YageRenderEnv -> IO (a)
runYageRenderer (YageRenderer a) env = runReaderT a env

--addRenderable :: SomeRenderable -> Yage ()
--addRenderable r = modify $ \s@YageState{..} -> s{ resources = YageResources (r : (renderables resources)) }

