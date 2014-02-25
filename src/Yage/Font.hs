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

import Yage.Font.FontTexture as FT
import Yage.Font.TextBuffer as TB

import           Yage.Rendering
import           Yage.Rendering.Transformation
import           Yage.Geometry




data RenderText = RenderText
    { _textIdent   :: Int
    , _textBuffer  :: TextBuffer
    , _textTexCh   :: TextureChannel
    , _textShader  :: ShaderResource
    , _textTransf  :: Transformation GLfloat
    } deriving (Typeable)

makeLenses ''RenderText


instance Renderable RenderText P2T2C4 where
    renderDefinition rt = 
        let fontTex     = rt^.textBuffer.tbufTexture
            name        = fontTex^.font.to fontname -- warning, not a good ident
            texImg      = TextureImage name (fontTex^.textureData)
            texDef      = [TextureDefinition (rt^.textTexCh) texImg]
            textMesh    = Right (rt^.textBuffer.tbufMesh)
        in RenderDefinition textMesh {--(rt^.textShader)--} texDef Triangles 
    renderTransformation rt = rt^.textTransf
