{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ParallelListComp   #-}
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
import Yage.Geometry.Elements
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

geometryFromOBJ :: OBJ -> (PosGeo, TexGeo)
geometryFromOBJ obj 
    | not $ hasTextureCoords obj  = error "OBJ is missing neccessary texture coords"
    | otherwise =
    let vertGeo = Geometry verts (V.map (fmap fVertexIndex) triFaces)
        texGeo  = Geometry texs  (V.map (fmap fTextureIndex) triFaces)
    in (vertGeo, texGeo)

    where
    verts   = V.map (fmap realToFrac . unGeoVertex)     $ obj^.vertexData.geometricVertices
    texs    = V.map (fmap realToFrac . unTextureVertex) $ obj^.vertexData.textureVertices
    elems   = error "undefined" -- undefined -- obj^.elements.faces
    
    triFaces :: V.Vector (Triangle OBJFaceVertex)
    triFaces = V.concatMap createFaceVert elems
    
    createFaceVert :: OBJ.Face -> V.Vector (Triangle OBJFaceVertex)
    createFaceVert (OBJ.Face (a:b:c:d:[])) = V.fromList . triangles $ Face (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c) (mkFaceVertex d)
    createFaceVert (OBJ.Face (a:b:c:[]))   = V.singleton $ Triangle (mkFaceVertex a) (mkFaceVertex b) (mkFaceVertex c)
    createFaceVert _ = error "Yage.Geometry.Formats.Obj: invalid Face in OBJ"

    mkFaceVertex :: References -> OBJFaceVertex
    mkFaceVertex (References ((OBJVertexIndex vi):(OBJTextureIndex ti):_)) = FaceVertex (vi-1) (ti-1)
    mkFaceVertex _ = error "Yage.Geometry.Formats.Obj.mkFaceVertex: invalid index order"


geometryFromOBJFile :: FilePath -> IO (PosGeo, TexGeo)
geometryFromOBJFile file = geometryFromOBJ <$> parseOBJFile file


hasTextureCoords :: OBJ -> Bool
hasTextureCoords obj = not . V.null $ obj^.vertexData.textureVertices

hasNormals :: OBJ -> Bool
hasNormals obj = not . V.null $ obj^.vertexData.vertexNormals
