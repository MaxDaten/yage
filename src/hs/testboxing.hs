{-# LANGUAGE ExistentialQuantification, StandaloneDeriving, MultiParamTypeClasses, FlexibleInstances #-}

module Test where

data TriMesh = TriMesh

data Lines = Lines

data Shader d = Shader (RenderData d -> IO ())


data RenderProgram d = RenderProgram
    { unprogram :: d -> IO () }

data RenderData d = RenderData
    { undata :: d }

data RenderDefinition d p = RenderDefinition
    { renderData :: RenderData d
    , renderProgram :: RenderProgram d p
    }

meshDef :: RenderDefinition TriMesh (Shader TriMesh)
meshDef = 
    let renderProg = RenderProgram shader
        renderData = RenderData TriMesh
    in  RenderDefinition renderData renderProg

shader :: TriMesh -> IO ()
shader _ = return ()