{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yage.Pipeline.Deferred.ResourceLoader where

import Yage.Prelude hiding (toList)
import Yage.Lens

import Yage.Resources
import Yage.Geometry

import                  Foreign.C.Types.Binary           ()
import                  Foreign.C.Types.DeepSeq          ()
import                  Data.Vinyl.Instances             ()
import qualified        Data.Map                         as M
import qualified        Data.Set                         as S

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


mkResourceLoader :: ( Storable (Vertex v), NFData (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> ResourceLoader (Vertex v)
mkResourceLoader fromInternal = ResourceLoader
    { objLoader = loadOBJ fromInternal
    , ygmLoader = loadYGM fromInternal
    }


loadOBJ :: ( Storable (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadOBJ fromInternal (filepath,subSelection) = do
    OBJ.GeometryGroup geoGroup <- OBJ.geometryFromOBJ <$> OBJ.parseOBJFile filepath
    createMesh $ M.mapKeys decodeUtf8 geoGroup
    
    where
    
    createMesh geoGroup
        | not $ isValidSelection subSelection geoGroup = error $ unpack $ format "invalid group selection: {}" (Only $ Shown $ subSelection S.\\ M.keysSet geoGroup)
        | otherwise = do
            let geos            = M.toList $ M.filterWithKey (isSelected subSelection) geoGroup
                tbnGeos         = over (traverse._2) (uncurry calcTangentSpaces) geos
                packed          = zipWith packer geos tbnGeos
                mesh            = emptyMesh & meshId .~ (encodeUtf8 $ fpToText filepath)
            return $ foldl' appendGeometry mesh packed
    
    converter p t n = fromInternal $ YGM.internalFormat p t n
    
    packer (ident, (pos, tex)) (_,tbn) = (encodeUtf8 ident, packGeos converter pos tex tbn)



loadYGM :: ( Storable (Vertex v), NFData (Vertex v) ) => (Vertex YGM.YGMFormat -> Vertex v) -> MeshFilePath -> IO (Mesh (Vertex v))
loadYGM fromInternal (filepath,subSelection) = createMesh <$> YGM.ygmFromFile filepath where
    createMesh YGM.YGM{..} 
        | not $ isValidSelection subSelection ygmModels = error $ unpack $ format "invalid group selection: {}" (Only $ Shown $ subSelection S.\\ M.keysSet ygmModels)
        | otherwise = 
            let geoMap = convertVertices <$> M.filterWithKey (isSelected subSelection) ygmModels
                mesh   = emptyMesh & meshId .~ encodeUtf8 ygmName
            in  M.foldlWithKey (\m k geo -> m `appendGeometry` (encodeUtf8 k, geo)) mesh geoMap

    convertVertices = geoVertices %~ map fromInternal


isSelected :: SubMeshSelection -> Text -> a -> Bool
isSelected selection key _ | S.null selection = True
                           | otherwise = key `S.member` selection
{-# INLINE isSelected #-}


isValidSelection :: SubMeshSelection -> Map Text a -> Bool
isValidSelection selection theMap = S.null $ S.difference selection (M.keysSet theMap)
{-# INLINE isValidSelection #-}
