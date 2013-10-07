{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections, CPP, DataKinds #-}
module GLFWTest where

import Prelude hiding (init)
import "GLFW-b" Graphics.UI.GLFW
import Control.Monad

main = do
    putStrLn "start"

    b <- init
    putStrLn $ "init: " ++ show b
    setErrorCallback $ Just errCB
    mwin <- createWindow 800 600 "GLFWTest" Nothing Nothing
    case mwin of
        Nothing -> putStrLn "errr"
        (Just win) -> forever $ render win
    return ()

render win = do
    makeContextCurrent $ Just win
    swapBuffers win

    shouldClose <- windowShouldClose win
    when shouldClose terminate

errCB err text = do
    putStrLn $ show err ++ " : " ++ text