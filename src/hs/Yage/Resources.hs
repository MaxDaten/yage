{-# LANGUAGE RecordWildCards #-}
module Yage.Resources where

import             Linear                          (V3(..), V4(..), zero)
import             Linear.Quaternion               (Quaternion)
import             Graphics.Rendering.OpenGL       (GLfloat)
import             Foreign.Storable
import             Foreign.Ptr                     (castPtr)
import             Control.Applicative             ((<$>), (<*>))

import             Debug.Trace
import             Yage.Import
---------------------------------------------------------------------------------------------------

type Orientation = Quaternion GLfloat
type Scale = V3 GLfloat

--type Vertex = V4 GLfloat
type Position = V4 GLfloat
type Normal = V4 GLfloat
type Color = V4 GLfloat
type Index = Int

data Vertex = Vertex 
    { position :: Position
    , normal   :: Normal
    --, color    :: Color
    } deriving (Show, Eq)

instance Storable Vertex where
    sizeOf _ = sizeOf (undefined::Position) + sizeOf (undefined::Normal)
    alignment _ = alignment (undefined::Position) + alignment (undefined::Normal)
    peek ptr =
        let ptr' = castPtr ptr
        in Vertex 
            <$> peekElemOff ptr' 0
            <*> peekElemOff ptr' 1

    poke ptr Vertex{..} = 
        let ptr' = castPtr ptr
        in do
            pokeElemOff ptr' 0 position
            pokeElemOff ptr' 1 normal

data TriMesh = TriMesh
    { meshId   :: !String
    , vertices :: ![Vertex]
    , indices  :: ![Index]
    , triCount :: !Int
    } deriving (Show)

instance Eq TriMesh where
    a == b = meshId a == meshId b

instance Ord TriMesh where
    compare a b = compare (meshId a) (meshId b)

mkTriMesh :: String -> [Vertex] -> [Index] -> TriMesh
-- TODO some assertions for invalid meshes
mkTriMesh id vs ixs | traceShow "here" False = undefined
                    | otherwise = TriMesh id vs ixs $ (length ixs) `quot` 3

---------------------------------------------------------------------------------------------------

data YageShaderResource = YageShaderResource
    { vert  :: FilePath
    , frag  :: FilePath
    } deriving (Show, Eq, Ord)

data RenderDefinition = RenderDefinition
    { defs :: (TriMesh, YageShaderResource)
    } deriving (Show, Eq, Ord)


