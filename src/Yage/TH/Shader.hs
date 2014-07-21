{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Yage.TH.Shader where

import Yage.Prelude hiding (lift)
import Yage.TH ()

import Yage.Rendering.Shader

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Instances.TH.Lift ()

import GLSL


shaderSrc :: FilePath -> ShaderType -> Q Exp
shaderSrc fp ty = 
    let file = fpToString fp 
    in [| ShaderSource file ty (unRaw $(glslRawFile file)) |]

vertexSrc :: FilePath -> Q Exp
vertexSrc fp = [|$(fp `shaderSrc` VertexShader)|]
    

fragmentSrc :: FilePath -> Q Exp
fragmentSrc fp = [|$(fp `shaderSrc` FragmentShader)|]


instance Lift ShaderSource where
    lift (ShaderSource name ty src) = [|ShaderSource name ty src|]

instance Lift ShaderType where
    lift VertexShader           = [|VertexShader|]
    lift TessControlShader      = [|TessControlShader|]
    lift TessEvaluationShader   = [|TessEvaluationShader|]
    lift GeometryShader         = [|GeometryShader|]
    lift FragmentShader         = [|FragmentShader|]
    lift ComputeShader          = [|ComputeShader|]