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
import                  Data.Vinyl.Universe.Field
import qualified        Data.Vector                      as V
import                  Data.Foldable                    (toList)

import "yage-geometry"  Yage.Geometry                    as Geometry

import                  Linear
import                  Yage.Rendering.Mesh
import                  Yage.Rendering.Vertex            as Vertex


type YPosition3 a = "vPosition" ::: V3 a
type YPosition2 a = "vPosition" ::: V2 a
type YTexture2  a = "vTexture"  ::: V2 a
type YTangentX  a = "vTangentX" ::: V3 a
type YTangentY  a = "vTangentY" ::: V3 a
type YTangentZ  a = "vTangentZ" ::: V4 a
type YColor4    a = "vColor"    ::: V4 a


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

tangentX :: SField (YTangentX a)
tangentX = SField

tangentZ :: SField (YTangentZ a)
tangentZ = SField

position3 :: SField (YPosition3 a)
position3 = SField

position2 :: SField (YPosition2 a)
position2 = SField

texture2 :: SField (YTexture2 a)
texture2 = SField

color4 :: SField (YColor4 a)
color4 = SField


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

    surfacesIndices = V.zipWith3 (\(GeoSurface p) (GeoSurface t) (GeoSurface n) -> V.zipWith3 mergeIndices p t n) (posG^.geoSurfaces) (texG^.geoSurfaces) (tbnG^.geoSurfaces)

    -- not the best implementation
    reindexSurfaces (surfsAccum, offset) surface =
      let surfLength = V.length surface
          mkTriangle i = Triangle (i*3+offset) (i*3+offset+1) (i*3+offset+2)
      in ( surfsAccum `V.snoc` ( GeoSurface $ V.generate surfLength mkTriangle ), offset + surfLength * 3 )

    emitVertex :: (Int, Int, Int) -> v
    emitVertex (vertIdx, texIdx, ntIdx) =
        vertexFormat (verts V.! vertIdx) (texs V.! texIdx) (norms V.! ntIdx)

    mergeIndices :: Triangle Int -> Triangle Int -> Triangle Int -> Triangle (Int, Int, Int)
    mergeIndices p tx tn  = (,,) <$> p <*> tx <*> tn

    verts = posG^.geoVertices
    texs  = texG^.geoVertices
    norms = tbnG^.geoVertices

    compatibleSurfaces =
        let posSurfaces  = posG^.geoSurfaces^..traverse.to (length.unGeoSurface)
            texSurfaces  = texG^.geoSurfaces^..traverse.to (length.unGeoSurface)
            normSurfaces = tbnG^.geoSurfaces^..traverse.to (length.unGeoSurface)
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
          ( Pos a -> Tex a -> TBN a -> (Vertex v) ) -> ByteString -> s (t (Vertex vert)) -> Mesh (Vertex v)
buildMesh vertexFormat name geo = 
    let vs     = concatMap (concatMap (vertices . triangles) . getSurface) $ surfaces geo
        posGeo = makeSimpleTriGeo $ V.map (rGet position3) $ V.fromList vs
        texGeo = makeSimpleTriGeo $ V.map (rGet texture2)  $ V.fromList vs
        tbnGeo = calcTangentSpaces posGeo texGeo
        
        triGeo = packGeos vertexFormat posGeo texGeo tbnGeo
    in meshFromTriGeo name triGeo

