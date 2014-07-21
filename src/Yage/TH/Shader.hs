{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.TH.Shader 
    ( module GLSL
    -- , module Yage.TH.Shader
    ) where

import Yage.Lens
import GLSL as GLSL
import qualified Yage.Rendering.Shader as S


instance S.HasShaderSource (ShaderSource VertexShader) where
    shaderSource = to get where
        get (ShaderSource name _ raw) = S.ShaderSource name S.VertexShader raw

instance S.HasShaderSource (ShaderSource FragmentShader) where
    shaderSource = to get where
        get (ShaderSource name _ raw) = S.ShaderSource name S.FragmentShader raw