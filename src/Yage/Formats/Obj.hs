{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ParallelListComp   #-}
module Yage.Formats.Obj
    ( module Yage.Formats.Obj
    , module Yage.Formats.Obj.Parser
    ) where

import Yage.Prelude hiding (any, toList)
import Yage.Lens hiding (elements)
import Yage.Math
import qualified Data.Vector as V
import Yage.Formats.Obj.Parser hiding (Face)
import qualified Yage.Formats.Obj.Parser as OBJ (Face)
import Yage.Geometry.Vertex
import Yage.Geometry.Elements
import Yage.Geometry




type PosGeo a = TriGeo (V3 a)
type TexGeo a = TriGeo (V2 a)
type Geo pn tn a = TriGeo (Vertex (P3T2 pn tn a))


type FaceIdx = Int
type VertIdx = Int
type TexIdx  = Int

data OBJFaceVertex = FaceVertex
    { fVertexIndex  :: !VertIdx
    , fTextureIndex :: !TexIdx
    }

geometryFromOBJ :: (Floating a, Enum a) => OBJ -> (PosGeo a, TexGeo a)
geometryFromOBJ obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise =
    let vertGeo = Geometry (V.map (fmap realToFrac) verts) (V.map (fmap fVertexIndex) triFaces)
        texGeo  = Geometry (V.map (fmap realToFrac) texs) (V.map (fmap fTextureIndex) triFaces)
    in (vertGeo, texGeo)

    where
    verts   = obj^.vertexData.geometricVertices
    texs    = obj^.vertexData.textureVertices
    elems   = obj^.elements.faces
    
    triFaces :: V.Vector (Triangle OBJFaceVertex)
    triFaces = V.concatMap createFaceVert elems
    
    createFaceVert :: OBJ.Face -> V.Vector (Triangle OBJFaceVertex)
    createFaceVert (a:b:c:d:[]) = V.fromList . triangles $ Face (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c) (mkFaceVertex d)
    createFaceVert (a:b:c:[])   = V.singleton $ Triangle (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c)
    createFaceVert _ = error "Yage.Geometry.Formats.Obj: invalid Face in OBJ"

    mkFaceVertex :: References -> OBJFaceVertex
    mkFaceVertex ((VertexIndex vi):(TextureIndex ti):_) = FaceVertex (vi-1) (ti-1)
    mkFaceVertex _ = error "Yage.Geometry.Formats.Obj.mkFaceVertex: invalid index order"


geometryFromOBJFile :: (Floating a, Enum a, Show a) => FilePath -> IO (PosGeo a, TexGeo a)
geometryFromOBJFile file = geometryFromOBJ <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
