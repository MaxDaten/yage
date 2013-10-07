module Yage.Rendering.Debug where

import Yage.Import

import Linear (vector)

import Yage.Rendering.Types


type DebugRenderable = SomeRenderable


data LineDebug = LineDebug
    { 

    }


genNormalRenderable :: SomeRenderable -> DebugRenderable
genNormalRenderable r = 
    let (mesh, shader) = renderDefinition r
        normalData      = makeNormalData mesh
    in SomeRenderable undefined --- TODO how to link with its base entity for world-local deps?
    where
        makeNormalData :: TriMesh -> TriMesh
        makeNormalData TriMesh{..} = 
            TriMesh { meshId = meshId + "__debug"
                    , vertices = concatMap mkNormalVerts vertices
                    , indices = indices
                    , triCount = !Int
                    }

        mkNormalVerts :: Vertex -> [Vertex]
        mkNormalVerts v@(Vertex position normal color) = 
            [v, Vertex (position + (vector normal)) normal color]