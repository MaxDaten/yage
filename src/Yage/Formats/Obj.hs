{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE DeriveGeneric               #-}
module Yage.Formats.Obj
    ( module Yage.Formats.Obj
    , module Yage.Formats.Obj.Parser, module OBJ
    ) where

import Yage.Prelude hiding (any, toList)
import Yage.Lens hiding (elements)
import Yage.Math
import qualified Data.Vector as V

import Yage.Formats.Obj.Parser hiding (Face)
import qualified Yage.Formats.Obj.Parser as OBJ (Face(..))
import Yage.Geometry.Elements hiding (Surface)
import Yage.Geometry



type PosGeo = TriGeo (V3 Float)
type TexGeo = TriGeo (V2 Float)

type FaceIdx = Int
type VertIdx = Int
type TexIdx  = Int

data OBJFaceVertex = FaceVertex
    { fVertexIndex  :: !VertIdx
    , fTextureIndex :: !TexIdx
    }

newtype GeometryGroup = GeometryGroup { geometryGroups :: Map ByteString (PosGeo, TexGeo) }
    deriving ( Show, Eq, Ord, Generic )


-- TODO reindex and sparse vertices
geometryFromOBJ :: OBJ -> GeometryGroup
geometryFromOBJ obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise = GeometryGroup $ map groupToGeos (obj^.groups)

    where
    -- TODO : extract only verts/texs for this group
    groupToGeos :: SmoothingGroups -> (PosGeo, TexGeo)
    groupToGeos (SmoothingGroups surfsMap) = 
        let surfs = foldl' ( \accum elems -> facesToIdxTri accum (elems^.faces) ) V.empty surfsMap
        in ( Geometry verts $ V.map (fmap.fmap $ fVertexIndex) surfs
           , Geometry texs  $ V.map (fmap.fmap $ fTextureIndex) surfs
           )

    verts   = V.map (fmap realToFrac . unGeoVertex)     $ obj^.vertexData.geometricVertices
    texs    = V.map (fmap realToFrac . unTextureVertex) $ obj^.vertexData.textureVertices
    
    facesToIdxTri :: (V.Vector (GeoSurface (Triangle OBJFaceVertex))) ->
                     V.Vector OBJ.Face ->  
                     (V.Vector (GeoSurface (Triangle OBJFaceVertex)))
    facesToIdxTri vertices faces = vertices `V.snoc` (GeoSurface $ V.concatMap createFaceVert faces)
    
    createFaceVert :: OBJ.Face -> V.Vector (Triangle OBJFaceVertex)
    createFaceVert (OBJ.Face (a:b:c:d:[])) = V.fromList . triangles $ Face (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c) (mkFaceVertex d)
    createFaceVert (OBJ.Face (a:b:c:[]))   = V.singleton $ Triangle (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c)
    createFaceVert _ = error "Yage.Geometry.Formats.Obj: invalid Face in OBJ"

    mkFaceVertex :: References -> OBJFaceVertex
    mkFaceVertex (References ((OBJVertexIndex vi):(OBJTextureIndex ti):_)) = FaceVertex (vi-1) (ti-1)
    mkFaceVertex _ = error "Yage.Geometry.Formats.Obj.mkFaceVertex: invalid index order"


geometryFromOBJFile :: FilePath -> IO GeometryGroup
geometryFromOBJFile file = geometryFromOBJ <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
