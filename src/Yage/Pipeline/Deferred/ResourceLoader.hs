{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude hiding (toList)

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Data.Vinyl.Instances             ()

import                  Yage.Rendering.Mesh


import qualified        Yage.Formats.Obj                 as OBJ
import qualified        Yage.Formats.Ygm                 as YGM

import                  Yage.Pipeline.Deferred.GeometryPass

import                  Linear


deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = mkResourceLoader fromInternal
    where
    fromInternal :: Vertex YGM.YGMFormat -> Vertex GeoVertex
    fromInternal internal =
        yposition3 =: ( realToFrac <$> ( rGet yposition3 internal :: V3 Float ) ) <+>
        ytexture2  =: ( realToFrac <$> ( rGet ytexture2 internal  :: V2 Float ) ) <+>
        ytangentX  =: ( realToFrac <$> ( rGet ytangentX internal  :: V3 Float ) ) <+>
        ytangentZ  =: ( realToFrac <$> ( rGet ytangentZ internal  :: V4 Float ) )


mkResourceLoader :: Storable (Vertex v) => (Vertex YGM.YGMFormat -> Vertex v) -> ResourceLoader v
mkResourceLoader fromInternal = ResourceLoader
    { objLoader = loadOBJ fromInternal
    , ygmLoader = loadYGM fromInternal
    }


loadOBJ :: Storable (Vertex v) => (Vertex YGM.YGMFormat -> Vertex v) -> FilePath -> IO (Mesh v)
loadOBJ fromInternal path = do
    (posGeo, texGeo) <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile path
    let tbnGeo          = calcTangentSpaces posGeo texGeo
        convert p t tbn = fromInternal $ YGM.internalFormat p t tbn
        packed          = packGeos convert posGeo texGeo tbnGeo
    return $ meshFromTriGeo ( fpToString path ) packed


loadYGM :: Storable (Vertex v) => (Vertex YGM.YGMFormat -> Vertex v) -> FilePath -> IO (Mesh v)
loadYGM fromInternal path = toMesh <$> YGM.ygmFromFile path
    where toMesh ygm = meshFromTriGeo ( unpack $ YGM.ygmName ygm ) $ fromInternal <$> YGM.ygmModel ygm


