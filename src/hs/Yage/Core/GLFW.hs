{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Yage.Core.GLFW where

-- Types
import qualified Data.Trie as T (Trie, empty, insert)
import qualified Data.ByteString.Char8 as BS (pack)

-- concepts
import           Control.Monad.State
import           Control.Monad.IO.Class
import           Control.Exception

-- 3rd party apis
import qualified Graphics.UI.GLFW as GLFW (init, terminate, Window, createWindow, destroyWindow)

-- own
import           EnableGUI
--import           Yage.Core.Types
--import           Yage.Core.API


newtype Application a = Application (StateT (ApplicationState) IO a)
    deriving (Monad, MonadIO, MonadState (ApplicationState))


data WindowState = WindowState
    { winTitle          :: !String
    , winWidth          :: !Int
    , winHeight         :: !Int
    , winRatio          :: !Double
    , win               :: !GLFW.Window
    }

data ApplicationState = ApplicationState
    { appTitle          :: !String
    , appWindows        :: T.Trie (WindowState)
    }

initialState :: ApplicationState
initialState = ApplicationState
    { appTitle = ""
    , appWindows = T.empty
    }


execApplication :: String -> Application a -> IO (Either String a)
execApplication title (Application a) = do
    inited <- GLFW.init
    if inited 
        then do
            (a, st') <- runStateT a $ initialState { appTitle = title }
            destroyAllWindows $ appWindows st'
            GLFW.terminate
            return $ Right a
        else do
            return $ Left "Init failed" 


destroyAllWindows :: T.Trie (WindowState) -> IO ()
destroyAllWindows ws = 
    (return $ fmap (GLFW.destroyWindow . win) ws) >> return ()


fmapM :: (Monad m, Functor f) => (a -> b) -> m (f a) -> m (f b)
fmapM = liftM . fmap



main :: IO ()
main = do
    e <- execApplication "my test app" app
    case e of
        Right r -> print r
        Left e -> print e

    where
        app :: Application Int
        app = do
            return 8
{--


instance AppImpl GLFW GLFW.Window IO where
    --execApplication (GLFW app) initState = runStateT app initState
    initApplication = liftIO $ GLFW.init
    disposeApplication = liftIO $ GLFW.terminate
    createWindow width height title = do
        mwin <- liftIO $ GLFW.createWindow width height title Nothing Nothing
        case mwin of
            Just win -> do
                appState <- get
                let winState = WindowState title width height 1 win
                put $ appState { appWindows = T.insert (BS.pack title) winState (appWindows appState) }
                return win
            Nothing -> fail "abc"

--}