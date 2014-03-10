module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Data.Proxy
import                  Data.Vinyl.Binary                ()

import                  Yage.Rendering.Mesh


import qualified        Yage.Geometry.Formats.Obj        as OBJ
import qualified        Yage.Geometry.Formats.Ygm        as YGM

import Yage.Pipeline.Deferred.Spec

deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = ResourceLoader
    { objLoader = loadOBJ
    , ygmLoader = loadYGM 
    }

loadOBJ :: FilePath -> IO (TriMesh GeoVertex)
loadOBJ path = do
    (posGeo, texGeo) <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile path
    let ntGeo        = genSmoothings posGeo texGeo
    return $ makeMeshGeo (fpToString path) $ packGeos YGM.vertexFormat posGeo texGeo ntGeo
    


loadYGM :: FilePath -> IO (TriMesh GeoVertex)
loadYGM path = toMesh <$> YGM.ygmFromFile path (Proxy::Proxy GeoVertex)
    where toMesh ygm = makeMeshGeo (unpack $ YGM.ygmName ygm) $ (YGM.ygmModel ygm)


