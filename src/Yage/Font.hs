{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import           Yage.Rendering.VertexSpec




data RenderText = RenderText
    { _textIdent   :: Int
    , _textBuffer  :: TextBuffer
    , _textTexCh   :: TextureChannel
    , _textShader  :: Program
    , _textAttribs :: MeshData Vertex2P4C2T -> [VertexAttribute]
    , _textTransf  :: RenderTransformation
    } deriving (Typeable)

makeLenses ''RenderText


instance Renderable RenderText where
    renderDefinition rt = 
        let fontTex     = rt^.textBuffer.tbufTexture
            name        = fontTex^.font.to fontname
            texImg      = TextureImage name (fontTex^.textureData)
            texDef      = [TextureDefinition (rt^.textTexCh) texImg]
            textMesh    = (makeMesh (rt^.textIdent) name (rt^.textBuffer.tbufMesh) (rt^.textAttribs)) 
                            & meshModToken %~ (`hashWithSalt` rt^.textBuffer.tbufText)
        in RenderDefinition textMesh (rt^.textShader) texDef Triangles 
    renderTransformation rt = rt^.textTransf
