{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PackageImports #-}
module Yage.Geometry
    ( module Vertex
    , module Geometry
    , module Data.Vinyl
    , module Yage.Geometry
    ) where

import                  Yage.Prelude                     hiding (toList)
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


--{--
packGeos :: (Epsilon a, Floating a) => 
          (Pos a -> Tex a -> TBN a -> v) -> 
          TriGeo (Pos a) -> 
          TriGeo (Tex a) -> 
          TriGeo (TBN a) ->
          TriGeo v
packGeos vertexFormat posG texG tbnG 
    | sameLength = error "can't merge geos, invalid number of elements"
    | otherwise =
        let mergedIdxs = V.zipWith3 mergeIndices (geoElements posG) (geoElements texG) (geoElements tbnG)
            vs         = V.concatMap ( V.fromList . toList . (fmap emitVertex) ) mergedIdxs
        in Geometry { geoVertices = vs
                    , geoElements = V.generate (length mergedIdxs) ( \i -> Triangle (i*3) (i*3+1) (i*3+2) )
                    }
    where

    emitVertex (vertIdx, texIdx, ntIdx) =
        vertexFormat (verts V.! vertIdx) (texs V.! texIdx) (norms V.! ntIdx)

    mergeIndices :: Triangle Int -> Triangle Int -> Triangle Int -> Triangle (Int, Int, Int)
    mergeIndices p tx tn  = (,,) <$> p <*> tx <*> tn

    verts = geoVertices posG
    texs  = geoVertices texG
    norms = geoVertices tbnG

    sameLength =
        let lp = length $ geoElements posG
            lt = length $ geoElements texG
            ln = length $ geoElements tbnG
        in lp /= lt || ln /= lt || lp /= ln


buildTriGeo :: (Foldable f, HasTriangles t, Epsilon a, Floating a) => 
               ( Pos a -> Tex a -> TBN a -> v ) 
            -> f (t (Pos a)) 
            -> f (t (Tex a)) 
            -> TriGeo v 
buildTriGeo vertexFormat pos tex =
    let posGeo = makeSimpleTriGeo' pos
        texGeo = makeSimpleTriGeo' tex
        tbnGeo = calcTangentSpaces posGeo texGeo
    in packGeos vertexFormat posGeo texGeo tbnGeo


buildMesh :: ( Epsilon a, Floating a
             , HasTriangles t, HasSurfaces s, Storable (Vertex v)
             , IElem (YPosition3 a) vert, IElem (YTexture2 a) vert ) => 
          ( Pos a -> Tex a -> TBN a -> (Vertex v) ) -> String -> s (t (Vertex vert)) -> Mesh v
buildMesh vertexFormat name geo = 
    let vs     = concatMap (concatMap (vertices . triangles) . getSurface) $ surfaces geo
        posGeo = makeSimpleTriGeo $ V.map (rGet yposition3) $ V.fromList vs
        texGeo = makeSimpleTriGeo $ V.map (rGet ytexture2)  $ V.fromList vs
        tbnGeo = calcTangentSpaces posGeo texGeo
        
        triGeo = packGeos vertexFormat posGeo texGeo tbnGeo
    in meshFromTriGeo name triGeo

