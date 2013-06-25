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

data YRenderConfig = YRenderConfig
    { clearColor    :: GL.Color4 Double
    }

data YageRenderEnv = YageRenderEnv
    { application   :: !YApplication
    , window        :: !YGLWindow
    , renderConfig  :: !YRenderConfig
    }

data YageState = YageState
    { inputs     :: Set.Set Input
    , renderEnv  :: YageRenderEnv
    }

newtype YageRenderer a = YageRenderer (ReaderT YageRenderEnv IO a)
    deriving (Functor, Monad, MonadIO, MonadReader YageRenderEnv, Typeable)

newtype Yage a = Yage (StateT YageState IO a)
    deriving (Functor, Monad, MonadIO, MonadState YageState, Typeable)

data Input = Input
type Inputs = Set.Set Input

data Scene = Scene
    deriving (Show)

type YageWire = WireM Yage

---------------------------------------------------------------------------------------------------

runYage :: YageState -> Yage a -> IO (a, YageState)
runYage st (Yage a) = runStateT a st


runYageRenderer :: YageRenderer a -> YageRenderEnv -> IO (a)
runYageRenderer (YageRenderer a) env = runReaderT a env


yageMain :: YageWire () Scene -> Session IO -> IO ()
yageMain wire session = do
    _ <- bracket 
        (initialization) 
        (finalization)
        (\st -> yageLoop st wire session)
    return ()


initialization :: IO (YageState)
initialization = do
    app <- mkApplication []
    win <- mkGLWindow
    
    resizeWindow win 800 600
    showWindow win

    let rEnv = YageRenderEnv app win (YRenderConfig (GL.Color4 0.3 0.3 0.3 0))
    return $ YageState Set.empty rEnv


finalization :: YageState -> IO ()
finalization _ = return ()


yageLoop :: YageState -> YageWire () Scene -> Session IO -> IO ()
yageLoop state wire session = do
    (dt, s') <- sessionUpdate session

    ins <- processInput (application $ renderEnv $ state)
    let st = state { inputs = ins }

    ((mx, w'), st') <- runYage st $ stepWire wire dt ()
    either handleError (drawScene' $ renderEnv st') mx

    yageLoop st' w' s'
    where
        handleError e = print $ "err:" ++ show e
        drawScene' env scene = runYageRenderer (drawScene scene) env

---------------------------------------------------------------------------------------------------

drawScene :: Scene -> YageRenderer ()
drawScene scene = (renderFrame scene) >> afterFrame


processInput :: YApplication -> IO (Inputs)
processInput app = do
    processEvents app
    return (Set.empty) -- dummy


afterFrame :: YageRenderer ()
afterFrame = io $ do
    performGC
    --threadDelay (16666)


renderFrame :: Scene -> YageRenderer ()
renderFrame scene = do
    beforeRender
    doRender scene
    afterRender


doRender :: Scene -> YageRenderer ()
doRender scene = return ()


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


io :: MonadIO m => IO a -> m a
io = liftIO

---------------------------------------------------------------------------------------------------

getRenderEnv :: Yage (YageRenderEnv)
getRenderEnv = gets renderEnv


putRenderEnv :: YageRenderEnv -> Yage ()
putRenderEnv env = get >>= \yst -> put yst{ renderEnv = env }


getRenderConfig :: Yage (YRenderConfig)
getRenderConfig = renderConfig `liftM` getRenderEnv

putRenderConfig :: YRenderConfig -> Yage ()
putRenderConfig conf = getRenderEnv >>= \env -> putRenderEnv env{ renderConfig = conf }