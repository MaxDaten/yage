{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main (main, spec) where

import Test.Hspec

import Yage.Prelude
import Yage.Math
import Yage.Lens hiding (elements)

import qualified Data.Vector as V
import Data.Vinyl.Instances ()

import Yage.Geometry.Vertex
import Yage.Geometry
import Yage.Geometry.Formats.Obj

type OBJVertex = P3T2 "pos" "tex" Float

squareOBJ :: OBJ
squareOBJ = mempty & vertexData.geometricVertices .~ V.fromList [ V3 0 2 0, V3 0 0 0, V3 2 0 0, V3 2 2 0 ]
                   & vertexData.vertexNormals     .~ V.fromList [ V3 0 0 1 ]
                   & vertexData.textureVertices   .~ V.fromList [ V2 0 0  , V2 0 1  , V2 1 0  , V2 1 1 ]
                   & elements.faces .~ V.singleton [[ VertexIndex 1, TextureIndex 1, NormalIndex 1 ]
                                                   ,[ VertexIndex 2, TextureIndex 2, NormalIndex 1 ]
                                                   ,[ VertexIndex 3, TextureIndex 4, NormalIndex 1 ]
                                                   ,[ VertexIndex 4, TextureIndex 3, NormalIndex 1 ]
                                                   ]

squareGeo :: TriGeo (Vertex OBJVertex)
squareGeo = Geometry
  { geoVertices = V.fromList [ position3 =: V3 0 2 0 <+> texture2 =: V2 0 0
                             , position3 =: V3 0 0 0 <+> texture2 =: V2 0 1
                             , position3 =: V3 2 0 0 <+> texture2 =: V2 1 1
                             
                             , position3 =: V3 0 2 0 <+> texture2 =: V2 0 0
                             , position3 =: V3 2 0 0 <+> texture2 =: V2 1 1
                             , position3 =: V3 2 2 0 <+> texture2 =: V2 1 0
                             ]
  , geoElements = V.fromList [Triangle 0 1 2, Triangle 3 4 5]
  }



main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "parse OBJ files" $ do
    it "parses a simple square" $ do
      parsedObj <- parseOBJFile $ "test" </> "res" </> "square.obj"
      parsedObj `shouldBe` squareOBJ

    it "parses a simple square into Geometry (drops normals)" $ do
      parsedGeo <- testPacker <$> (geometryFromOBJFile $ "test" </> "res" </> "square.obj")
      parsedGeo `shouldBe` squareGeo

  where
  testFormat :: V3 Float -> V2 Float -> Vertex OBJVertex 
  testFormat p t = position3 =: p <+> texture2 =: t

  testPacker :: (TriGeo (V3 Float), TriGeo (V2 Float)) -> TriGeo (Vertex OBJVertex)
  testPacker = uncurry (packGeos2 testFormat)