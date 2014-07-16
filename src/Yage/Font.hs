{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module Yage.Font
    ( module Yage.Font
    , module FT
    , module TB
    , module FTExport
    ) where

import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)

import Yage.Prelude
import Yage.Lens

import Yage.Transformation
import Yage.Rendering
import Yage.Font.FontTexture as FT
import Yage.Font.TextBuffer as TB

import           Yage.Uniforms.MVP


type FontUniforms       = PerspectiveUniforms ++ '[ YModelMatrix ]
type FontTextureField   = '[TextureUniform "FontTexture"]
type FontShaderData     = ShaderData FontUniforms FontTextureField

data RenderText = RenderText
    { _textIdent        :: Int
    , _textBuffer       :: TextBuffer
    , _textShaderData   :: FontShaderData
    , _textShader       :: ShaderResource
    , _textTransf       :: Transformation Float
    } deriving ( Typeable )

makeLenses ''RenderText

{--

instance Renderable RenderText GlyphVertex where
    renderDefinition rt = 
        let fontTex     = rt^.textBuffer.tbufTexture
            theName     = fontTex^.font.to fontname -- warning, not a good ident
            texImg      = Texture2D theName (fontTex^.textureData)
            texDef      = [TextureDefinition (rt^.textTexCh) texImg]
            textMesh    = rt^.textBuffer.tbufMesh
            drawSett    = GLDrawSettings Triangles (Just Back)
        in RenderEntity textMesh shaderData drawSettings 
--}
