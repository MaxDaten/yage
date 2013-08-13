module Yage.Rendering.Primitives where

import Control.Applicative ((<$>))
import Control.Lens ((^.))

import Linear (point)

import Yage.Import
import Yage.Resources

import Linear (V3(..), V4(..), R3(_xyz), cross, signorm, vector)


cubeMesh :: TriMesh
cubeMesh = 
    let verts = point 
                <$> [ V3 (-0.5) 0.5 0.5   , V3 0.5 0.5 0.5   , V3 0.5 (-0.5) 0.5   , V3 (-0.5) (-0.5) 0.5
                    , V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5)
                    ]
        frontFace = [0, 3, 2, 0, 2, 1] :: [Int]
        leftFace  = [0, 4, 7, 0, 7, 3] :: [Int]
        rightFace = [1, 2, 6, 1, 6, 5] :: [Int]
        topFace   = [0, 1, 4, 4, 1, 5] :: [Int]
        bottomFace= [7, 2, 3, 7, 6, 2] :: [Int]
        ixs       = frontFace
                  ++ leftFace
                  ++ topFace
                  ++ reverse (map (+4) frontFace) -- back
                  ++ rightFace
                  ++ bottomFace
    in mkTriMeshfromSpare "cube" (traceShow' verts) (traceShow' ixs) (V4 1.0 1.0 1.0 1.0)

quadMesh :: TriMesh
quadMesh = 
    let verts = point 
                <$> [ V3 (-0.5) 0.5 0.0, V3 (-0.5) (-0.5) 0.0, V3 0.5 (-0.5) 0.0, V3 0.5 0.5 0.0 ]
        ixs   = [0, 1, 2, 0, 2, 3]
        color = V4 1.0 1.0 1.0 1.0
    in mkTriMeshfromSpare "quad" verts ixs color

mkTriMeshfromSpare :: String -> [Position] -> [Index] -> Color -> TriMesh
mkTriMeshfromSpare id verts ixs color = traceShow' $ mkTriMesh id (processSpareVerts verts ixs color) (take (length ixs) [0..])


-- | takes spare 3d-points (without duplicates) and the indices
-- to construct the adequate attributes to be processed by opengl 
processSpareVerts :: [Position] -> [Index] -> Color -> [Vertex]
processSpareVerts vs ixs color = genNormals $ extract vs ixs
    where 
      extract :: [Position] -> [Index] -> [Position]
      extract vs = map (vs!!)

      genNormals :: [Position] -> [Vertex]
      genNormals vs =
        let ns = concat $ map (\(a:b:xs) -> replicate 3 $ genNormal a b) $ splitEvery 3 vs
            cs = repeat color
        in zipWith3 Vertex vs ns cs

      -- | generate a normal with cross product from two vectors and flip the normal to
      -- show from the inside to the outside
      genNormal :: Position -> Position -> Normal
      genNormal v1 v2 = signorm $ (v1^._xyz) `cross` (v2^._xyz)

