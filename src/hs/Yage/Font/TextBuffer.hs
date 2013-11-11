{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Yage.Font.TextBuffer where

import Yage.Prelude hiding (Text)
import Yage.Images
import Yage.Math
import GHC.Float
import Foreign.C.Types


import Data.Map hiding (map, null)
import Data.List (map, sortBy, null)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Digest.Pure.SHA
import Control.Lens hiding (indices)

import Graphics.Font as FT hiding (width, height)
import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)

import Yage.Texture.Atlas
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives
import Linear

import Codec.Picture.Types

import Yage.Font.FontTexture


type Caret = V2 Double
data TextBuffer = TextBuffer
    { _tbufTexture     :: FontTexture
    , _tbufRenderDef   :: RenderDefinition
    , _tbufCaret       :: Caret
    , _tbufText        :: Text
    }

makeLenses ''TextBuffer

pixelFormat = 64.0
            --let i = showDigest . sha1 $ T.encodeUtf8 text

emptyTextBuffer :: FontTexture -> Program -> TextBuffer
emptyTextBuffer fTex prog = TextBuffer fTex makeDef (V2 0 0) ""
    where
        makeDef = 
            let fname = fontname $ fTex^.font
            in RenderDefinition
                { def'ident     = "textbuffer-"-- TODO 
                , def'data      = emptyMesh "textbuffer-" -- (textToMesh fTex text){ ident = i }
                , def'program   = prog
                , def'textures  = [TextureDefinition (0, "textures") (TextureImage fname (fTex^.textureData))]
                }


pushChar :: TextBuffer -> Char -> TextBuffer
pushChar tbuf '\n' =
    let face    = fontFace theFont
        theFont = tbuf^.tbufTexture.font
        fsize   = fromI ((charSize $ fontDescr theFont)^._2) / pixelFormat
        hSpace  = tbuf^.tbufTexture.fontMarkup.verticalSpacing
        lineH   = hSpace * fsize * fromI (lineHeight face)
    in tbufCaret._y -~ lineH / pixelFormat $
       tbufCaret._x .~ 0
       $ tbuf

pushChar tbuf c = 
    let mesh            = def'data $ tbuf^.tbufRenderDef
        (caret', mesh') = aux mesh (getFontDataFor c)
    in tbufRenderDef %~  (\rd -> rd{def'data = mesh'}) $
       tbufCaret     .~  caret' $
       tbufText      <>~ T.singleton c
       $ tbuf
    where
        getFontDataFor c = tbuf^.tbufTexture.charRegionMap.at c 
        aux mesh (Just fdata@(glyph, region)) =
            let fTex          = tbuf^.tbufTexture
                caret         = tbuf^.tbufCaret
                metric        = glyphMetrics glyph
                vSpace        = tbuf^.tbufTexture.fontMarkup.horizontalSpacing
                advance       = vSpace * fromI (glyHoriAdvance metric) / pixelFormat
                (texW, texH)  = (dynamicMap imageWidth (fTex^.textureData), dynamicMap imageHeight (fTex^.textureData)) 
                (w,h)         = (fromI $ region^.to width, fromI $ region^.to height)
                mesh'         = mesh `pushToBack` (makeGlypMesh caret fdata texW texH)
            in (caret & _x +~ advance, mesh')
        aux mesh Nothing = aux mesh (getFontDataFor '_') -- WARNING - can lead to endless recursion (FIXME: fallback font with error chars)


writeText :: TextBuffer -> Text -> TextBuffer
writeText tbuf = T.foldl pushChar tbuf


makeGlypMesh :: Caret -> FontData -> Int -> Int -> Mesh Vertex4342
makeGlypMesh caret (gly, r) tw th =
        let GlyphMetrics{..}   = traceShow' $ glyphMetrics gly
            bearingX = fromI (glyHoriBearingX) / pixelFormat
            bearingY = fromI (glyHoriBearingY) / pixelFormat
            
            leftX    = caret^._x + bearingX
            topY     = caret^._y + bearingY
            
            w        = fromI $ r^.to width
            h        = fromI $ r^.to height

            u0       = fromI (r^.x0) / fromI tw
            u1       = fromI (r^.x1) / fromI tw
            v0       = fromI (r^.y1) / fromI th
            v1       = fromI (r^.y0) / fromI th
        in Mesh 
             { ident = ""
             , vertices = [ vert leftX     topY       u0 v1
                          , vert leftX     (topY - h) u0 v0
                          , vert (leftX+w) (topY - h) u1 v0
                          , vert (leftX+w) topY       u1 v1
                          ]
             , indices  = [0, 1, 2, 2, 3, 0]
             , triCount = 2
             }
        where
            vert :: Double -> Double -> Double -> Double -> Vertex4342
            vert x y u v =
                Vertex (V4 (CFloat $ double2Float x) (CFloat $ double2Float y) 0 1 ) 
                       (V3 0 1 0)
                       (V4 0 0 0 0)
                       (V2 (CFloat $ double2Float u) (CFloat $ double2Float v))


fromI :: (Integral a, Num b) => a -> b
fromI = fromIntegral
