{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
module Yage.Font.TextBuffer where

import           Yage.Prelude              hiding (Text)
import           Yage.Lens                 hiding (cons)

import           Yage.Math
-----------------------------------------------------------------------------------------
import qualified Data.Vector.Storable      as V
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
-----------------------------------------------------------------------------------------
-- import           Graphics.Font             as FT hiding (height, width)
-----------------------------------------------------------------------------------------
import           Yage.Rendering
import           Yage.Geometry
import           Yage.Images
-----------------------------------------------------------------------------------------
import           Yage.Font.FontTexture
-----------------------------------------------------------------------------------------


type GlyphVertex = Y'P2TX2C4 GLfloat

type Caret      = V2 Double
type CharColor  = V4 Float

data TextBuffer = TextBuffer
    { _tbufTexture      :: FontTexture
    , _tbufMesh         :: Mesh (Vertex GlyphVertex)
    , _tbufCaret        :: Caret
    , _tbufCurrentColor :: CharColor
    , _tbufText         :: Text
    } deriving ( Typeable )

makeLenses ''TextBuffer

pixelFormat :: Double
pixelFormat = 64.0

pxNorm :: FontDescriptor -> (Double, Double)
pxNorm (FontDescriptor _ (resX,resY)) = (fromI resX, fromI resY)



emptyTextBuffer :: FontTexture -> TextBuffer
emptyTextBuffer fTex =
    TextBuffer
    { _tbufTexture      = fTex
    , _tbufMesh         = emptyMesh
    , _tbufCaret        = 0
    , _tbufCurrentColor = V4 0.0 0.0 0.0 1.0
    , _tbufText         = ""
    }

-- the api

buffText :: Lens' TextBuffer Text
buffText = lens _tbufText setText

charColor :: Lens' TextBuffer CharColor
charColor = tbufCurrentColor

setText :: TextBuffer -> Text -> TextBuffer
setText = writeText . clearTextBuffer

writeText :: TextBuffer -> Text -> TextBuffer
writeText = T.foldl pushChar


clearTextBuffer :: TextBuffer -> TextBuffer
clearTextBuffer fTex =
    emptyTextBuffer (fTex^.tbufTexture)
        & tbufCurrentColor .~ (fTex^.tbufCurrentColor)

-- function mostly for internal use

pushChar :: TextBuffer -> Char -> TextBuffer
pushChar tbuf '\n' =
    let face            = tbuf^.tbufTexture.fontMetric.fontFace
        hSpace          = tbuf^.tbufTexture.fontMetric.fontMarkup.verticalSpacing
        lineH           = hSpace * fromI (lineHeight face)
        em              = fromI $ unitsPerEM face
        (_, ptY)        = over both fromI $ tbuf^.tbufTexture.fontMetric.fontDescriptor.to charSize
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
            let fTex            = tbuf^.tbufTexture
                caret           = tbuf^.tbufCaret
                color           = tbuf^.tbufCurrentColor

                metric          = glyphMetrics glyph
                vSpace          = tbuf^.tbufTexture.fontMetric.fontMarkup.horizontalSpacing

                advance         = vSpace * fromI (glyHoriAdvance metric)
                norm@(normX,_)  = tbuf^.tbufTexture.fontMetric.fontDescriptor.to pxNorm

                texDim          = fmap (+1) $ imageDimension $ fTex^.fontMap
                -- (w,h)         = (fromI $ region^.to width, fromI $ region^.to height)
                glyphVerts      = makeGlypMesh caret color fdata texDim norm
                glypIdx         :: V.Vector Int
                glypIdx         = V.fromList [0, 1, 2, 0, 2, 3]
                glypComp        = makeComponent ((c:show caret)^.packedChars) glypIdx
            in ( caret & _x +~ advance / normX
               , mesh `appendComponent` (glypComp, V.fromList glyphVerts)
               )
        aux mesh Nothing = aux mesh (getFontDataFor '_') -- FIXME: WARNING - can lead to endless recursion (FIXME: fallback font with error chars)


makeGlypMesh :: Caret -> CharColor -> CharData -> V2 Int -> (Double, Double) -> [Vertex GlyphVertex]
makeGlypMesh caret color (gly, region) dim' (normX, normY) =
        let GlyphMetrics{..}   = glyphMetrics gly
            bearingX = fromI glyHoriBearingX / normX
            bearingY = fromI glyHoriBearingY / normY
            dim      = fromI <$> dim'

            leftX    = caret^._x + bearingX
            topY     = caret^._y + bearingY

            w        = fromI glyWidth / normX
            h        = fromI glyHeight / normY

            V2 u0 v0 = (fromI <$> region^.xy1) / dim
            V2 u1 v1 = (fromI <$> region^.xy2) / dim
        -- origin of gl-textures is bottom left
        in [ vert leftX     topY       u0 (1 - v0)
           , vert leftX     (topY - h) u0 (1 - v1)
           , vert (leftX+w) (topY - h) u1 (1 - v1)
           , vert (leftX+w) topY       u1 (1 - v0)
           ]
        where
            vert :: Double -> Double -> Double -> Double -> Vertex GlyphVertex
            vert x y u v = position2 =: (V2 (realToFrac x) (realToFrac y))
                        <+> texture2 =: (V2 (realToFrac u) (realToFrac v))
                        <+> color4   =: (fmap realToFrac color)


fromI :: (Integral a, Num b) => a -> b
fromI = fromIntegral


instance Show TextBuffer where
    show tb =
        show $ format "TextBuffer: font = {}, \"{}\""
            ( Shown $ tb^.tbufTexture.fontMetric.fontName
            , Shown $ tb^.tbufText
            )
