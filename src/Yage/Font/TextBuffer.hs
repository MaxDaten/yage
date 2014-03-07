{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Yage.Font.TextBuffer where

import           Yage.Prelude              hiding (Text)
import           Yage.Lens

import           Yage.Math
-----------------------------------------------------------------------------------------
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
-----------------------------------------------------------------------------------------
import           Graphics.Font             as FT hiding (height, width)
-----------------------------------------------------------------------------------------
import           Codec.Picture.Types
-----------------------------------------------------------------------------------------
import           Yage.Rendering
import qualified Yage.Geometry             as Vert (P2TX2C4)
-----------------------------------------------------------------------------------------
import           Yage.Font.FontTexture
-----------------------------------------------------------------------------------------


type GlyphVertex = Vert.P2TX2C4

type Caret = V2 Double
data TextBuffer = TextBuffer
    { _tbufTexture :: FontTexture
    , _tbufMesh    :: TriMesh GlyphVertex
    , _tbufCaret   :: Caret
    , _tbufText    :: Text
    } deriving (Typeable)

makeLenses ''TextBuffer

pixelFormat :: Double
pixelFormat = 64.0

pxNorm :: FontDescriptor -> (Double, Double)
pxNorm (FontDescriptor _ (resX,resY)) = (fromI resX, fromI resY)



emptyTextBuffer :: FontTexture -> TextBuffer
emptyTextBuffer fTex = TextBuffer fTex emptyMesh (V2 0 0) ""

clearTextBuffer :: TextBuffer -> TextBuffer
clearTextBuffer fTex = emptyTextBuffer (fTex^.tbufTexture)

pushChar :: TextBuffer -> Char -> TextBuffer
pushChar tbuf '\n' =
    let face            = fontFace theFont
        theFont         = tbuf^.tbufTexture.font
        hSpace          = tbuf^.tbufTexture.fontMarkup.verticalSpacing
        lineH           = hSpace * fromI (lineHeight face)
        em              = fromI $ unitsPerEM face
        (_, ptY)        = fromI <$$> tbuf^.tbufTexture.fontDescriptor.to charSize
    -- line height is in font units, 
    -- this is different to the glyph metric unit (which is 26.6 format) and must
    -- scaled with the underlying dpi
    -- normalize line height with em
    -- line height * ptY = line height in pt (/pixelformat to convert from 26.6 format)
    in tbufCaret._y -~ lineH / em * ptY / pixelFormat $ 
       tbufCaret._x .~ 0
       $ tbuf

pushChar tbuf c =
    let mesh            = tbuf^.tbufMesh
        (caret', mesh') = aux mesh (getFontDataFor c)
    in tbuf & tbufMesh      .~  mesh'
            & tbufCaret     .~  caret'
            & tbufText      <>~ T.singleton c
    where
        getFontDataFor c = tbuf^.tbufTexture.charRegionMap.at c
        aux mesh (Just fdata@(glyph, _region)) =
            let fTex          = tbuf^.tbufTexture
                caret         = tbuf^.tbufCaret 

                metric        = glyphMetrics glyph
                vSpace        = tbuf^.tbufTexture.fontMarkup.horizontalSpacing

                advance       = vSpace * fromI (glyHoriAdvance metric)
                norm@(normX,_)= pxNorm . fontDescr $ tbuf^.tbufTexture.font

                (texW, texH)  = ( dynamicMap imageWidth (fTex^.textureData)
                                , dynamicMap imageHeight (fTex^.textureData)
                                )
                -- (w,h)         = (fromI $ region^.to width, fromI $ region^.to height)
                glyphVerts    = makeGlypMesh caret fdata (texW, texH) norm
                vcnt          = vertexCount mesh
                mesh'         = pushToBack mesh glyphVerts [Triangle (vcnt+0) (vcnt+1) (vcnt+2), Triangle (vcnt+2) (vcnt+3) (vcnt+0)]
            in (caret & _x +~ advance / normX, mesh')
        aux mesh Nothing = aux mesh (getFontDataFor '_') -- WARNING - can lead to endless recursion (FIXME: fallback font with error chars)


setText :: TextBuffer -> Text -> TextBuffer
setText = writeText . clearTextBuffer

writeText :: TextBuffer -> Text -> TextBuffer
writeText = T.foldl pushChar


makeGlypMesh :: Caret -> FontData -> (Int, Int) -> (Double, Double) -> [Vertex GlyphVertex]
makeGlypMesh caret (gly, region) (tw, th) (normX, normY) =
        let GlyphMetrics{..}   = glyphMetrics gly
            bearingX = fromI glyHoriBearingX / normX
            bearingY = fromI glyHoriBearingY / normY

            leftX    = caret^._x + bearingX
            topY     = caret^._y + bearingY

            w        = fromI glyWidth / normX
            h        = fromI glyHeight / normY

            u0       = fromI (region^.x0) / fromI tw
            u1       = fromI (region^.x1) / fromI tw
            v0       = fromI (region^.y1) / fromI th
            v1       = fromI (region^.y0) / fromI th
        in [ vert leftX     topY       u0 v1
           , vert leftX     (topY - h) u0 v0
           , vert (leftX+w) (topY - h) u1 v0
           , vert (leftX+w) topY       u1 v1
           ]
        where
            vert :: Double -> Double -> Double -> Double -> Vertex GlyphVertex
            vert x y u v = position2 =: V2 (realToFrac x) (realToFrac y)
                        <+> texture2 =: V2 (realToFrac u) (realToFrac v)
                        <+> color4   =: 0


fromI :: (Integral a, Num b) => a -> b
fromI = fromIntegral
