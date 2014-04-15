{-# LANGUAGE RecordWildCards #-}
module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude hiding (toList)

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Data.Proxy
import                  Data.Vinyl.Instances             ()

import                  Yage.Rendering.Mesh


import qualified        Yage.Formats.Obj        as OBJ
import qualified        Yage.Formats.Ygm        as YGM

import                  Yage.Pipeline.Deferred.GeometryPass

deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = ResourceLoader
    { objLoader = loadOBJ
    , ygmLoader = loadYGM 
    }


loadOBJ :: FilePath -> IO (Mesh GeoVertex)
loadOBJ path = do
    (posGeo, texGeo) <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile path
    let ntGeo        = genSmoothings posGeo texGeo
    return $ meshFromTriangleGeometry ( fpToString path ) $ packGeos3 YGM.vertexFormat posGeo texGeo ntGeo
    


loadYGM :: FilePath -> IO (Mesh GeoVertex)
loadYGM path = toMesh <$> YGM.ygmFromFile path (Proxy::Proxy GeoVertex)
    where toMesh ygm = meshFromTriangleGeometry ( unpack $ YGM.ygmName ygm ) $ YGM.ygmModel ygm
