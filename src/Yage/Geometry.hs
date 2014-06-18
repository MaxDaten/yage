{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Yage.Geometry
    ( module Vertex
    , module Geometry
    , module Data.Vinyl
    , module Yage.Geometry
    ) where

import                  Yage.Prelude                     hiding (toList)
import                  Yage.Lens

import                  Data.Vinyl
import qualified        Data.Vector                      as V
import                  Data.Foldable                    (toList)

import                  Yage.Geometry.Vertex             as Vertex

import "yage-geometry"  Yage.Geometry                    as Geometry

import                  Linear
import                  Yage.Rendering.Mesh


type YPosition3 a = Position3 "vPosition" a
type YPosition2 a = Position2 "vPosition" a
type YTexture2  a = Texture2  "vTexture"  a
type YTangentX  a = Tangent3  "vTangentX" a
type YTangentY  a = Tangent3  "vTangentY" a
type YTangentZ  a = Tangent4  "vTangentZ" a
type YColor4    a = Color4    "vColor"    a


-- 3D
type Y'P3 a       = '[ YPosition3 a ]
type Y'P3TX2 a    = [ YPosition3 a
                    , YTexture2 a
                    ]

type Y'P3TX2TN a  = [ YPosition3 a
                    , YTexture2 a
                    , YTangentX a
                    , YTangentZ a
                    ]

type Y'P3TX2TNC4 a = [ YPosition3 a
                     , YTexture2 a
                     , YTangentX a
                     , YTangentZ a
                     , YColor4 a
                     ]

-- 2D
type Y'P2 a        = '[ YPosition2 a ]

type Y'P2TX2 a     = [ YPosition2 a
                     , YTexture2 a
                     ]

type Y'P2TX2C4 a   = [ YPosition2 a
                     , YTexture2 a
                     , YColor4 a
                     ]

ytangentX :: YTangentX a
ytangentX = Field

ytangentZ :: YTangentZ a
ytangentZ = Field

yposition3 :: YPosition3 a
yposition3 = Field

ytexture2 :: YTexture2 a
ytexture2 = Field


-- | 
packGeos :: forall a v. ( Epsilon a, Floating a ) => 
          (Pos a -> Tex a -> TBN a -> v) -> 
          TriGeo (Pos a) -> 
          TriGeo (Tex a) -> 
          TriGeo (TBN a) ->
          TriGeo v
packGeos vertexFormat posG texG tbnG
    | not compatibleSurfaces = error "packGeos: invalid surfaces"
    | otherwise = Geometry
        { _geoVertices = V.concatMap (V.concatMap (V.fromList . toList . fmap emitVertex)) surfacesIndices
        -- trivial indices, just like [[0..n], [n+1..m], ...]
        , _geoSurfaces = fst $ V.foldl' reindexSurfaces (V.empty, 0) surfacesIndices
        } 
    where

    surfacesIndices = V.zipWith3 (V.zipWith3 mergeIndices) (posG^.geoSurfaces) (texG^.geoSurfaces) (tbnG^.geoSurfaces)

    -- not the best implementation
    reindexSurfaces (surfsAccum, offset) surface =
      let surfLength = V.length surface
          mkTriangle i = Triangle (i*3+offset) (i*3+offset+1) (i*3+offset+2)
      in ( surfsAccum `V.snoc` ( V.generate surfLength mkTriangle ), offset + surfLength * 3 )

    emitVertex :: (Int, Int, Int) -> v
    emitVertex (vertIdx, texIdx, ntIdx) =
        vertexFormat (verts V.! vertIdx) (texs V.! texIdx) (norms V.! ntIdx)

    mergeIndices :: Triangle Int -> Triangle Int -> Triangle Int -> Triangle (Int, Int, Int)
    mergeIndices p tx tn  = (,,) <$> p <*> tx <*> tn

    verts = posG^.geoVertices
    texs  = texG^.geoVertices
    norms = tbnG^.geoVertices

    compatibleSurfaces =
        let posSurfaces  = posG^.geoSurfaces^..traverse.to length
            texSurfaces  = texG^.geoSurfaces^..traverse.to length
            normSurfaces = tbnG^.geoSurfaces^..traverse.to length
        in posSurfaces == texSurfaces && posSurfaces == normSurfaces


buildTriGeo :: ( Foldable f, HasTriangles t, Epsilon a, Floating a ) => 
               ( Pos a -> Tex a -> TBN a -> v ) 
            -> f (t (Pos a)) 
            -> f (t (Tex a)) 
            -> TriGeo v 
buildTriGeo vertexFormat pos tex =
    let posGeo = makeSimpleTriGeoF pos
        texGeo = makeSimpleTriGeoF tex
    in packGeos vertexFormat posGeo texGeo $ calcTangentSpaces posGeo texGeo


buildMesh :: ( Epsilon a, Floating a
             , HasTriangles t, HasSurfaces s, Storable (Vertex v)
             , IElem (YPosition3 a) vert, IElem (YTexture2 a) vert ) => 
          ( Pos a -> Tex a -> TBN a -> (Vertex v) ) -> Text -> s (t (Vertex vert)) -> Mesh (Vertex v)
buildMesh vertexFormat name geo = 
    let vs     = concatMap (concatMap (vertices . triangles) . getSurface) $ surfaces geo
        posGeo = makeSimpleTriGeo $ V.map (rGet yposition3) $ V.fromList vs
        texGeo = makeSimpleTriGeo $ V.map (rGet ytexture2)  $ V.fromList vs
        tbnGeo = calcTangentSpaces posGeo texGeo
        
        triGeo = packGeos vertexFormat posGeo texGeo tbnGeo
    in meshFromTriGeo name triGeo

