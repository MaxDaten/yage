{-# LANGUAGE TemplateHaskell #-}
module Yage.Font
    ( module Yage.Font
    , module FTExport
    ) where

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

import Graphics.Font as FT
import Graphics.Font as FTExport (FontLoadMode(..), FontDescriptor(..), Font, loadFont)

import Yage.Texture.Atlas
import Yage.Rendering
import Yage.Rendering.VertexSpec
import Yage.Rendering.Primitives
import Linear

import Codec.Picture.Types

---------------------------------------------------------------------------------------------------


data FontTexture a = FontTexture
    { _fontName          :: String
    , _charRegionMap     :: RegionMap Char
    , _textureData       :: DynamicImage
    }

makeLenses ''FontTexture

makeFontTexture :: String -> TextureAtlas Char Pixel8 -> FontTexture a
makeFontTexture fontname filedAtlas = FontTexture
    { _fontName      = fontname
    , _charRegionMap = regionMap filedAtlas
    , _textureData   = ImageY8 $ atlasToImage filedAtlas
    }

generateFontTexture :: Font -> FontLoadMode -> [Char] -> TextureAtlas Char Pixel8 -> Either [(Char, AtlasError Char)] (FontTexture Pixel8)
generateFontTexture font mode chars emptyAtlas =
    let imgs          = sortBy descArea $ (map (generateCharImg font mode) chars) `piz` chars
        (err, atlas)  = insertImages imgs emptyAtlas
        name          = fontname font
    in if null err then Right $ makeFontTexture name atlas else Left err 
    where
        descArea (_, img1) (_, img2) = descending imageByAreaCompare img1 img2



data TextBuffer a = TextBuffer
    { _fbufTexture     :: FontTexture a
    , _fbufRenderDef   :: RenderDefinition
    }

makeLenses ''TextBuffer


simpleTextBuffer :: FontTexture a -> Program -> Text -> TextBuffer a
simpleTextBuffer fTex prog text = TextBuffer fTex makeTextDef
    where
        makeTextDef = 
            let i = showDigest . sha1 $ T.encodeUtf8 text
            in RenderDefinition
            { def'ident     = i
            , def'data      = traceShow' $ (textToMesh fTex text){ ident = i }
            , def'program   = prog
            , def'textures  = [TextureDefinition (0, "textures") (TextureImage (fTex^.fontName) (fTex^.textureData))]
            }


textToMesh :: FontTexture a -> Text -> Mesh Vertex4342
textToMesh fTex = snd . T.foldl (flip pushCharToMesh) (0, emptyMesh "")
    where
        pushCharToMesh :: Char -> (Double, Mesh Vertex4342) -> (Double, Mesh Vertex4342)
        pushCharToMesh c (caret, mesh) =
            let Just charRegion = fTex^.charRegionMap.at c -- ascii null?
                (texW, texH)    = (dynamicMap imageWidth (fTex^.textureData), dynamicMap imageHeight (fTex^.textureData)) 
                (w,h)           = (fromIntegral $ charRegion^.to width, fromIntegral $ charRegion^.to height)
                mesh'           =  mesh `pushToBack` (makeGlypMesh caret charRegion texW texH)
            in (caret + w, mesh')


makeGlypMesh :: Double -> Rectangle -> Int -> Int -> Mesh Vertex4342
makeGlypMesh caret r tw th =
        let w  = fromIntegral $ r^.to width
            h  = fromIntegral $ r^.to height
            u0 = fromIntegral (r^.x0) / fromIntegral tw
            u1 = fromIntegral (r^.x1) / fromIntegral tw
            v0 = fromIntegral (r^.y1) / fromIntegral th
            v1 = fromIntegral (r^.y0) / fromIntegral th
        in Mesh 
             { ident = ""
             , vertices = [ vert caret     h u0 v1
                          , vert caret     0 u0 v0
                          , vert (caret+w) 0 u1 v0
                          , vert (caret+w) h u1 v1
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
