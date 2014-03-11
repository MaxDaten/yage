{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Yage.Font
    ( module Yage.Font
    , module FT
    , module TB
    , module FTExport
    ) where

import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)

import Yage.Prelude
import Yage.Lens

import Yage.Font.FontTexture as FT
import Yage.Font.TextBuffer as TB

import           Yage.Rendering
import           Yage.Rendering.Transformation




data RenderText = RenderText
    { _textIdent   :: Int
    , _textBuffer  :: TextBuffer
    , _textTexCh   :: TextureChannel
    , _textShader  :: ShaderResource
    , _textTransf  :: Transformation GLfloat
    } deriving (Typeable)

makeLenses ''RenderText


instance Renderable RenderText GlyphVertex where
    renderDefinition rt = 
        let fontTex     = rt^.textBuffer.tbufTexture
            theName     = fontTex^.font.to fontname -- warning, not a good ident
            texImg      = TextureImage theName (fontTex^.textureData)
            texDef      = [TextureDefinition (rt^.textTexCh) texImg]
            textMesh    = rt^.textBuffer.tbufMesh
            drawSett    = GLDrawSettings Triangles (Just Back)
        in RenderEntity textMesh drawSett {--(rt^.textShader)--} texDef 
