{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import Yage.Prelude
import Yage.Core.Application
import Yage.Rendering.Shader

import Filesystem.Path

import qualified Yage.Core.OpenGL as GL


config :: ApplicationConfig
config = defaultAppConfig

hints :: [WindowHint]
hints =
    [ WindowHint'ContextVersionMajor  4
    , WindowHint'ContextVersionMinor  1
    , WindowHint'OpenGLProfile        OpenGLProfile'Core
    , WindowHint'OpenGLForwardCompat  True
    , WindowHint'Visible              False
    ]


-- | currently just printing the GLUtil ShaderProgram
main :: IO ()
main = do
    (shaderFile0::FilePath, shaderFile1::FilePath) <- readArgs

    execApplication "ShaderTool" config $ do
        win <- createWindowWithHints hints 800 600 "Context Window"
        withWindowAsCurrent win $ \w -> do
            prog <- io $ GL.loadShaderProgram $ map shaderPairs [ shaderFile0, shaderFile1 ]
            printTF "loaded Shader:\n0: {}\n1: {}\n{}\n\n" ( Shown shaderFile0
                                                           , Shown shaderFile1
                                                           , Shown prog
                                                           )
        destroyWindow win

    where

    shaderPairs :: FilePath -> (GL.ShaderType, String)
    shaderPairs fp =
        case extension fp of
            Just "vert" -> (GL.VertexShader, fpToString fp)
            Just "frag" -> (GL.FragmentShader, fpToString fp)
            Just ext    -> error $ "invalid shader extension" ++ show ext ++ ". Onnly .vert and .frag accepted"
            Nothing     -> error "shaderfile without extension"
