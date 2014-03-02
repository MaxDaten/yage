module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude
import Yage.Lens

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Data.Proxy
import                  Data.Vinyl.Binary                ()
import qualified        Data.Vector                      as V

import                  Yage.Rendering.Mesh


import qualified        Yage.Geometry.Formats.Obj        as OBJ
import qualified        Yage.Geometry.Formats.Ygm        as YGM

import Yage.Pipeline.Deferred.Spec

deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = ResourceLoader
    { objLoader = loadOBJ
    , ygmLoader = \path -> loadYGM <$> (YGM.ygmFromFile path (Proxy::Proxy GeoVertex))
    }

loadOBJ :: OBJ.OBJ -> Mesh P3T2
loadOBJ = makeMeshV' . geoToV . OBJ.geometryFromOBJ (Proxy::Proxy P3T2)

loadYGM :: YGM.YGM P3T2 -> Mesh P3T2
loadYGM ygm = makeMeshV (unpack $ YGM.ygmName ygm) $ geoToV . YGM.ygmModel $ ygm


geoToV :: Geometry (Triangle (Vertex a)) -> V.Vector (Vertex a)
geoToV = V.concatMap (V.fromList . triToVert) . geoElements
    where
    triToVert :: Triangle (Vertex a) -> [Vertex a]
    triToVert t = t^..traverse

