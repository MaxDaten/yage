module Yage.Rendering.Primitives where

import Yage.Import
import Yage.Rendering
import Yage.Rendering.Scene
import Yage.Types

import Linear (V3(..))


cubeMesh :: TriMesh
cubeMesh = let verts = [ V3 (-0.5) 0.5 0.5   , V3 0.5 0.5 0.5   , V3 0.5 (-0.5) 0.5   , V3 (-0.5) (-0.5) 0.5
                       , V3 (-0.5) 0.5 (-0.5), V3 0.5 0.5 (-0.5), V3 0.5 (-0.5) (-0.5), V3 (-0.5) (-0.5) (-0.5)
                       ]
               frontFace = [0, 3, 2, 0, 2, 1] :: [Int]
               leftFace  = [0, 4, 7, 0, 7, 3] :: [Int]
               ixs       = frontFace
                        ++ reverse (map (+4) frontFace) -- back
                        ++ leftFace
                        ++ reverse (map (+1) leftFace) -- right
        in mkTriMesh verts ixs

