{-# LANGUAGE RecordWildCards, TupleSections #-}
module Main where

import Control.Applicative
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Yage.Core.Raw.FFI
import Yage.Resources
import Yage.Rendering.Primitives
import Foreign.Storable (sizeOf)
import             Graphics.GLUtil
import qualified   Graphics.Rendering.OpenGL       as GL
import             Graphics.Rendering.OpenGL.GL    (($=))
import Graphics.Rendering.OpenGL.GL.Shaders.Program

import Debug.Trace

positionAttrib = "position"
fragmentColor = "gl_FragColor"


main :: IO ()
main = do
    app <- mkApplication []
    win <- mkGLWindow

    resizeWindow win 800 600
    showWindow win

    loop app win [] 0

    where
        loop app win renderables frame = do
            processEvents app

            renderables <- render win renderables frame

            loop app win renderables (succ frame)

        render win rs' frame = do
            beginDraw win
            preRender win

            rs <- if null rs' then sequence [mkObj cubeMesh] else return rs'
            mapM_ renderObj rs

            endDraw win
            return rs

        preRender win = do
            GL.clearColor $= GL.Color4 0.3 0.3 0.3 1.0
            GL.clear [GL.ColorBuffer]

            w <- width win
            h <- height win
            r <- return . floor =<< pixelRatio win
            GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )

        renderObj (sprog, vao) = withVAO vao $ do
            currentProgram $= Just (program sprog)
            drawIndexedTris 12
            currentProgram $= Nothing

        mkObj :: TriMesh -> IO (ShaderProgram, VAO)
        mkObj TriMesh{..} = do
            sprog <- loadShaderProgram "src/glsl/base.vert" "src/glsl/base.frag"
            traceIO $ show $ attribs sprog

            vao <- makeVAO $ do
                -- buffer vertext data
                vbo <- makeBuffer GL.ArrayBuffer vertices
                GL.bindBuffer GL.ArrayBuffer $= Just vbo

                -- set attributes for shaders
                enableAttrib sprog positionAttrib
                let stride = fromIntegral $ sizeOf (undefined::Vertex) * 3
                    vad = GL.VertexArrayDescriptor 3 GL.Float stride offset0
                setAttrib sprog positionAttrib GL.ToFloat vad
                
                -- set indices for rendering
                ebo <- bufferIndices $ map fromIntegral indices
                GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
            return (sprog, vao) 

