{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yage.Font.TextBuffer where

import           Foreign.C.Types
import           GHC.Float
import           Yage.Images
import           Yage.Math
import           Yage.Prelude              hiding (Text)
-----------------------------------------------------------------------------------------
import           Control.Lens              hiding (indices)
import           Data.Digest.Pure.SHA
import           Data.List                 (map, null, sortBy)
import           Data.Map                  hiding (map, null)
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
import qualified Data.Text.Lazy.Encoding   as T
-----------------------------------------------------------------------------------------
import           Graphics.Font             as FT hiding (height, width)
import           Graphics.Font             as FTExport (Font,
                                                        FontDescriptor (..),
                                                        FontLoadMode (..),
                                                        loadFont)
-----------------------------------------------------------------------------------------
import           Linear
import           Codec.Picture.Types
-----------------------------------------------------------------------------------------
import           Yage.Rendering
import           Yage.Rendering.Primitives
import           Yage.Rendering.VertexSpec
import           Yage.Texture.Atlas
-----------------------------------------------------------------------------------------
import           Yage.Font.FontTexture
-----------------------------------------------------------------------------------------




type Caret = V2 Double
data TextBuffer = TextBuffer
    { _tbufTexture :: FontTexture
    , _tbufMesh    :: MeshData Vertex2P2T4C
    , _tbufCaret   :: Caret
    , _tbufText    :: Text
    }

makeLenses ''TextBuffer

pixelFormat = 64.0
            --let i = showDigest . sha1 $ T.encodeUtf8 text

emptyTextBuffer :: FontTexture -> TextBuffer
emptyTextBuffer fTex = TextBuffer fTex emptyMeshData (V2 0 0) ""


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
    let mesh            = tbuf^.tbufMesh
        (caret', mesh') = aux mesh (getFontDataFor c)
    in tbufMesh      .~  mesh' $
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

                (texW, texH)  = ( dynamicMap imageWidth (fTex^.textureData)
                                , dynamicMap imageHeight (fTex^.textureData)
                                )
                (w,h)         = (fromI $ region^.to width, fromI $ region^.to height)

                mesh'         = mesh `pushToBack` (makeGlypMesh caret fdata texW texH)
            in (caret & _x +~ advance, mesh')
        aux mesh Nothing = aux mesh (getFontDataFor '_') -- WARNING - can lead to endless recursion (FIXME: fallback font with error chars)


writeText :: TextBuffer -> Text -> TextBuffer
writeText tbuf = T.foldl pushChar tbuf


makeGlypMesh :: Caret -> FontData -> Int -> Int -> MeshData Vertex2P2T4C
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
        in MeshData
             { vertices = [ vert leftX     topY       u0 v1
                          , vert leftX     (topY - h) u0 v0
                          , vert (leftX+w) (topY - h) u1 v0
                          , vert (leftX+w) topY       u1 v1
                          ]
             , indices  = [0, 1, 2, 2, 3, 0]
             , triCount = 2
             }
        where
            vert :: Double -> Double -> Double -> Double -> Vertex2P2T4C
            vert x y u v =
                Vertex (V2 (CFloat $ double2Float x) (CFloat $ double2Float y))
                       ()
                       (V4 0 0 0 0)
                       (V2 (CFloat $ double2Float u) (CFloat $ double2Float v))


fromI :: (Integral a, Num b) => a -> b
fromI = fromIntegral
