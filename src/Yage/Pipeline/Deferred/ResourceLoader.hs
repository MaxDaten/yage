{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude hiding (toList)
import Yage.Lens

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Data.Vinyl.Instances             ()
import                  Data.Map                         as M ( toList, filterWithKey, mapKeys )

import                  Yage.Rendering.Mesh


import qualified        Yage.Formats.Obj                 as OBJ
import qualified        Yage.Formats.Ygm                 as YGM

import                  Yage.Pipeline.Deferred.GeometryPass

import                  Linear


deferredResourceLoader :: ResourceLoader GeoVertex
deferredResourceLoader = mkResourceLoader fromInternal
    where
    fromInternal :: Vertex YGM.YGMFormat -> GeoVertex
    fromInternal internal =
        yposition3 =: ( realToFrac <$> ( rGet yposition3 internal :: V3 Float ) ) <+>
        ytexture2  =: ( realToFrac <$> ( rGet ytexture2 internal  :: V2 Float ) ) <+>
        ytangentX  =: ( realToFrac <$> ( rGet ytangentX internal  :: V3 Float ) ) <+>
        ytangentZ  =: ( realToFrac <$> ( rGet ytangentZ internal  :: V4 Float ) )


mkResourceLoader :: ( Storable (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> ResourceLoader (Vertex v)
mkResourceLoader fromInternal = ResourceLoader
    { objLoader = loadOBJ fromInternal
    , ygmLoader = loadYGM fromInternal
    }


loadOBJ :: ( Storable (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadOBJ fromInternal (filepath,subSelection) = do
    OBJ.GeometryGroup geoGroup <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile filepath
    let geos            = M.toList . M.filterWithKey (isSelected subSelection) . M.mapKeys decodeUtf8 $ geoGroup
        tbnGeos         = over (traverse._2) (uncurry calcTangentSpaces) geos
        packed          = zipWith packer geos tbnGeos
        mesh            = emptyMesh & meshId .~ fpToText filepath
    return $ foldl' appendGeometry mesh packed
    where
    converter p t n = fromInternal $ YGM.internalFormat p t n
    
    packer (ident, (pos, tex)) (_,tbn) = (ident, packGeos converter pos tex tbn)
    
    isSelected []        _ _   = True
    isSelected selection key _ = key `elem` selection




loadYGM :: Storable (Vertex v) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadYGM fromInternal (filepath,_subs) = toMesh <$> YGM.ygmFromFile filepath
    where toMesh ygm = meshFromTriGeo ( YGM.ygmName ygm ) $ fromInternal <$> YGM.ygmModel ygm


