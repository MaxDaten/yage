{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yage.TH.Shader
    ( module GLSL
    -- , module Yage.TH.Shader
    ) where

import Yage.Lens
import GLSL as GLSL
import qualified Data.ByteString.Char8 as BS
import qualified Yage.Rendering.Shader as S


instance S.HasShaderSource (ShaderSource VertexShader) where
    shaderSource = to get where
        get (ShaderSource name _ raw) = S.ShaderSource name S.VertexShader (BS.pack raw)

instance S.HasShaderSource (ShaderSource FragmentShader) where
    shaderSource = to get where
        get (ShaderSource name _ raw) = S.ShaderSource name S.FragmentShader (BS.pack raw)
