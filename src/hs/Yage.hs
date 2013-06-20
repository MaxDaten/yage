{-# LANGUAGE UnicodeSyntax, RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Yage where
 
import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Data.Bits
import Control.Concurrent
import Control.Monad
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable

import System.Mem

import Data.Set as Set

--import Graphics.Rendering.OpenGL.Raw as GL
--import Core.OpenGL as GL
import Graphics.Rendering.OpenGL.GL
import Yage.Core.Raw.FFI


data YageEnvironment = YageEnvironment
    { application   :: !YApplication
    , window        :: !YGLWindow
    }

data YageState = YageState

newtype Yage a = Yage (ReaderT YageEnvironment (StateT YageState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState YageState, MonadReader YageEnvironment, Typeable)

data Input = Input
type Inputs = Set.Set Input

initState :: YageState
initState = YageState

runYage :: YageEnvironment -> YageState -> Yage a -> IO (a, YageState)
runYage env st (Yage a) = runStateT (runReaderT a env) st


yageMain :: IO ()
yageMain = do
    _ <- bracket 
        (initialization) 
        (finalization)
        (mainLoop)
    return ()


initialization :: IO YageEnvironment
initialization = do
    app <- mkApplication []

    win <- mkGLWindow
    resizeWindow win 800 600
    showWindow win 

    return $ YageEnvironment app win

finalization :: YageEnvironment -> IO ()
finalization _ = return ()

mainLoop :: YageEnvironment -> IO (a, YageState)
mainLoop env = runYage env initState $ forever processFrame

processFrame :: Yage ()
processFrame = processInput >>= renderFrame >> afterFrame

processInput :: Yage (Inputs)
processInput = do
    withApplication $ io . processEvents
    return Set.empty

afterFrame :: Yage ()
afterFrame = io $ do
    performGC
    threadDelay (16666)

renderFrame :: Inputs -> Yage ()
renderFrame _ = do
    beforeRender
    render
    afterRender

render :: Yage ()
render = do
    let c = abs(sin $ fromIntegral 3 / 100.0)
    io $ clearColor $= Color4 1 c c 0

    withWindow $ io . \win -> do
        w <- width win
        h <- height win
        r <- return . floor =<< pixelRatio win
        viewport $= ((Position 0 0), (Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


beforeRender :: Yage ()
beforeRender = do
    beginRender
    io $ clear [ColorBuffer]

afterRender :: Yage ()
afterRender = endRender

beginRender :: Yage ()
beginRender = withWindow $ \w -> io . beginDraw $ w

endRender :: Yage ()
endRender = withWindow $ \w -> io . endDraw $ w


withWindow :: (YGLWindow -> Yage a) -> Yage a
withWindow f = asks window >>= f

withApplication :: (YApplication -> Yage a) -> Yage a
withApplication f = asks application >>= f

io :: MonadIO m => IO a -> m a
io = liftIO

