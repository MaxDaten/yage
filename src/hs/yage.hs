{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
module Main where
 
import FFI
import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Data.Bits
import Control.Concurrent
import Control.Monad

import System.Mem
import System.Exit


--import Graphics.Rendering.OpenGL.Raw as GL
import Graphics.Rendering.OpenGL.Raw.Core.Core43 as GL

main :: IO ()
main = appMain

appMain :: IO ()
appMain = do
    app <- mkApplication []
    --print app
    
    window <- mkGLWindow
    resizeWindow window 800 600
    showWindow window 

    loop app window 0


loop :: YApplication -> YGLWindow -> Int -> IO ()
loop app window i = do
    beginDraw window

    let c = abs(sin $ fromIntegral i / 100.0)
    --print $ show c
    glClearColor c c c 0.0

    w <- width window
    h <- height window
    r <- return . floor =<< pixelRatio window
    setViewport 0 0 (r * w) (r * h)
    --printDebug

    GL.glClear $ GL.gl_COLOR_BUFFER_BIT .|. GL.gl_DEPTH_BUFFER_BIT .|. GL.gl_STENCIL_BUFFER_BIT
    
    endDraw window    
    processEvents app
    performGC
    threadDelay (16666)

    loop app window (i+1)


printDebug :: IO ()
printDebug = do
    vp <- getViewport
    print $ show vp
    printGLError

    cc <- getClearColor
    print $ show cc

    v <- getGLVersion
    print $ show v

printGLError :: IO ()
printGLError = do
    err <- GL.glGetError
    when (err /= 0) $ do
        print . show $ err
        exitWith $ ExitFailure . fromIntegral $ err

setViewport :: Int -> Int -> Int -> Int -> IO ()
setViewport x y w h = GL.glViewport (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

getViewport :: IO ([Int])
getViewport = withArray inArr $ \arr -> do
    GL.glGetIntegerv GL.gl_VIEWPORT arr
    outArr <- peekArray (length inArr) arr
    return $ map fromIntegral outArr
    where inArr = (map fromIntegral [0, 0, 0, 0]) :: [GLint]

getClearColor :: IO ([Double])
getClearColor = withArray inArr $ \arr -> do
    GL.glGetDoublev GL.gl_COLOR_CLEAR_VALUE arr
    outArr <- peekArray (length inArr) arr
    return $ map realToFrac outArr
    where inArr = (map realToFrac [0, 0, 0, 0]) :: [GLdouble]

-- subject to code optimization
getGLVersion :: IO ((Int, Int))
getGLVersion = do
    major <- major'
    minor <- minor'
    return (major, minor)
    where 
        major' = alloca $ \i -> do
                GL.glGetIntegerv GL.gl_MAJOR_VERSION i
                return . fromIntegral =<< peek i
        minor' = alloca $ \i -> do
                GL.glGetIntegerv GL.gl_MINOR_VERSION i
                return . fromIntegral =<< peek i

