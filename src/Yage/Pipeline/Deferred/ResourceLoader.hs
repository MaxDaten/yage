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
loadOBJ path = toMesh <$> OBJ.parseOBJFile path
    where toMesh = makeMeshGeo (fpToString path) . geoWithNormTang . OBJ.geometryFromOBJ (Proxy::Proxy P3TX2)


loadYGM :: FilePath -> IO (TriMesh GeoVertex)
loadYGM path = toMesh <$> YGM.ygmFromFile path (Proxy::Proxy GeoVertex)
    where toMesh ygm = makeMeshGeo (unpack $ YGM.ygmName ygm) $ (YGM.ygmModel ygm)


geoWithNormTang :: TriGeo (Vertex P3TX2) -> TriGeo (Vertex P3TX2NT3)
geoWithNormTang = 
    genSmoothings (position3 :: YPosition3)
                  (texture2  :: YTexture2)
                  (normal3   :: YNormal3)
                  (tangent3  :: YTangent3)



