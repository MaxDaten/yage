module Yage.Rendering.Primitives where

import Yage.Import
import Yage.Resources

import Linear (V3(..))


cubeMesh :: TriMesh
cubeMesh = 
    let verts = [ V3 (-0.5) 0.5 0.5   , V3 0.5 0.5 0.5   , V3 0.5 (-0.5) 0.5   , V3 (-0.5) (-0.5) 0.5
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
    in mkTriMesh verts ixs

quadMesh :: TriMesh
quadMesh = 
    let verts = [ V3 (-0.5) 0.5 0.0, V3 (-0.5) (-0.5) 0.0, V3 0.5 (-0.5) 0.0, V3 0.5 0.5 0.0 ]
        ixs   = [0, 1, 2, 0, 2, 3]
    in mkTriMesh verts ixs
