{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Yage.Font.Buffer where

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

import Yage.Font



data TextBuffer = TextBuffer
    { _fbufTexture     :: FontTexture
    , _fbufRenderDef   :: RenderDefinition
    }

makeLenses ''TextBuffer


simpleTextBuffer :: FontTexture -> Program -> Text -> TextBuffer
simpleTextBuffer fTex prog text = TextBuffer fTex makeTextDef
    where
        makeTextDef = 
            let i = showDigest . sha1 $ T.encodeUtf8 text
            in RenderDefinition
            { def'ident     = i
            , def'data      = (textToMesh fTex text){ ident = i }
            , def'program   = prog
            , def'textures  = [TextureDefinition (0, "textures") (TextureImage (fTex^.fontName) (fTex^.textureData))]
            }


textToMesh :: FontTexture -> Text -> Mesh Vertex4342
textToMesh fTex = snd . T.foldl (flip pushCharToMesh) (0, emptyMesh "")
    where
        pushCharToMesh :: Char -> (Double, Mesh Vertex4342) -> (Double, Mesh Vertex4342)
        pushCharToMesh c (caret, mesh) = aux  $ fTex^.charRegionMap.at c  
            where
                aux (Just fdata@(glyph, region)) =
                    let metric        = glyphMetrics glyph
                        advance       = fromI (glyHoriAdvance metric) / 64.0
                        (texW, texH)  = (dynamicMap imageWidth (fTex^.textureData), dynamicMap imageHeight (fTex^.textureData)) 
                        (w,h)         = (fromI $ region^.to width, fromI $ region^.to height)
                        mesh'         = mesh `pushToBack` (makeGlypMesh caret fdata texW texH)
                    in (caret + advance, mesh')
                aux Nothing = pushCharToMesh '_' (caret, mesh) -- WARNING - can lead to endless recursion (FIXME: fallback font with error chars)


makeGlypMesh :: Double -> FontData -> Int -> Int -> Mesh Vertex4342
makeGlypMesh caret (gly, r) tw th =
        let GlyphMetrics{..}   = glyphMetrics gly
            bearingX = fromI (glyHoriBearingX) / 64.0
            bearingY = fromI (glyHoriBearingY) / 64.0
            
            leftX    = caret + bearingX
            topY     = h - (h - bearingY)
            
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
