{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import Yage.Prelude
import Yage.Core.Application
import Yage.Rendering.Shader as Shader
import qualified Data.Map.Strict as M

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
            prog <- io $ Shader.loadShaderProgram =<< mapM shaderPairs [ shaderFile0, shaderFile1 ]
            printTF "loaded Shader:\n0: {}\n1: {}\n{}\n\n" ( Shown shaderFile0
                                                           , Shown shaderFile1
                                                           , Shown prog
                                                           )
            print "attributes:"
            mapM_ printField (M.toList $ GL.attribs prog)
            print "uniforms:"
            mapM_ printField (M.toList $ GL.uniforms prog)
            let glProg = GL.program prog
            unis <- io $ GL.get (GL.activeUniforms glProg)
            io $ print unis
        destroyWindow win

    where

    printField (name, (loc, t)) =
        printTF "{}:{}:{}\n" (Shown loc, Shown name, Shown t)

    shaderPairs :: FilePath -> IO (GL.ShaderType, ByteString)
    shaderPairs fp =
        case extension fp of
            Just "vert" -> (GL.VertexShader,) <$> readFile fp
            Just "frag" -> (GL.FragmentShader,) <$> readFile fp
            Just ext    -> error $ "invalid shader extension" ++ show ext ++ ". Onnly .vert and .frag accepted"
            Nothing     -> error "shaderfile without extension"
