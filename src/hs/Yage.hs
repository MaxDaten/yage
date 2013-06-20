{-# LANGUAGE UnicodeSyntax, RecordWildCards, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveFunctor #-}
module Yage where
 
import Prelude hiding (id, (.)) -- reimported by Control.Wire

import Foreign
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Control.Concurrent
import Control.Monad
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Control.Wire hiding (Position, window)
import Data.Typeable

import System.Mem

import qualified Data.Set as Set

import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=)) 
import Yage.Core.Raw.FFI


data YageEnvironment = YageEnvironment
    { application   :: !YApplication
    , window        :: !YGLWindow
    }

data YageState = YageState
    { inputs     :: Set.Set Input
    , clearColor :: GL.Color4 Double
    }

newtype YageReader a = YageReader (ReaderT YageEnvironment IO a)
    deriving (Functor, Monad, MonadIO, MonadReader YageEnvironment, Typeable)

newtype Yage a = Yage (ReaderT YageEnvironment (StateT YageState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState YageState, MonadReader YageEnvironment, Typeable)

data Input = Input
type Inputs = Set.Set Input

data Scene = Scene
    deriving (Show)

type YageWire = WireM Yage

initState :: YageState
initState = YageState Set.empty (GL.Color4 0 0 0 0)


runYage :: YageEnvironment -> YageState -> Yage a -> IO (a, YageState)
runYage env st (Yage a) = runStateT (runReaderT a env) st


runYageEnvironment :: YageReader a -> YageEnvironment -> IO (a)
runYageEnvironment (YageReader a) env = runReaderT a env


yageMain :: YageWire () Scene -> Session IO -> IO ()
yageMain wire session = do
    _ <- bracket 
        (initialization) 
        (finalization)
        (\env -> yageLoop env initState wire session)
    return ()


initialization :: IO (YageEnvironment)
initialization = do
    app <- mkApplication []

    win <- mkGLWindow
    resizeWindow win 800 600
    showWindow win 

    return $ YageEnvironment app win


finalization :: YageEnvironment -> IO ()
finalization _ = return ()


yageLoop :: YageEnvironment -> YageState -> YageWire () Scene -> Session IO -> IO ()
yageLoop env state wire session = do
    (dt, s') <- sessionUpdate session

    ins <- processInput env
    let state = state { inputs = ins }

    ((mx, w'), st') <- runYage env state $ stepWire wire dt ()
    print $ show mx
    either handleError drawScene' mx
    yageLoop env st' w' s'
    where
        handleError e = print $ "err:" ++ show e
        drawScene' scene = runYageEnvironment (drawScene scene) env


drawScene :: Scene -> YageReader ()
drawScene scene = (renderFrame scene) >> afterFrame
{--
mainLoop :: YageEnvironment -> IO (a, YageState)
mainLoop env = runYage env initState $ forever processFrame


processFrame :: Yage ()
processFrame = processInput >>= renderFrame >> afterFrame
--}

processInput :: YageEnvironment -> IO (Inputs)
processInput env = do
    processEvents $ application env
    return Set.empty



afterFrame :: YageReader ()
afterFrame = io $ do
    performGC
    threadDelay (16666)


renderFrame :: Scene -> YageReader ()
renderFrame _ = do
    beforeRender
    render
    afterRender


render :: YageReader ()
render = do
    withWindow $ io . \win -> do
        w <- width win
        h <- height win
        r <- return . floor =<< pixelRatio win
        GL.viewport $= ((GL.Position 0 0), (GL.Size (fromIntegral (r * w)) (fromIntegral (r * h))) )


beforeRender :: YageReader ()
beforeRender = do
    beginRender
    --cc <- gets clearColor
    --io $ GL.clearColor $= fmap realToFrac cc
    io $ GL.clear [GL.ColorBuffer]


afterRender :: YageReader ()
afterRender = endRender


beginRender :: YageReader ()
beginRender = withWindow $ \w -> io . beginDraw $ w


endRender :: YageReader ()
endRender = withWindow $ \w -> io . endDraw $ w


withWindow :: (YGLWindow -> YageReader a) -> YageReader a
withWindow f = asks window >>= f


withApplication :: (YApplication -> YageReader a) -> YageReader a
withApplication f = asks application >>= f


io :: MonadIO m => IO a -> m a
io = liftIO

